import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.*
import org.scalajs.dom.window.history

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.JSON
import scala.util.Random
case class Filter(label: String, path: String, filter: List[TodoItem] => List[TodoItem] = identity)
private val FILTERS = List(
  Filter("All", "/"),
  Filter("Active", "/active", _.filterNot(_.completed)),
  Filter("Completed", "/completed", _.filter(_.completed))
)
type Key = String
def createKey(): Key = Random.nextDouble().toString
enum Action {
  case Add(title: String)
  case Toggle(key: Key)
  case Edit(key: Key)
  case Destroy(key: Key)
  case Save(title: String)
  case ToggleAll
  case ClearCompleted
  case ExitEdit
}
case class TodoItem(key: Key, title: String, completed: Boolean)
trait TodoItemObject extends js.Object {
  val title: String
  val completed: Boolean
}
case class State(todos: List[TodoItem], toggleAll: Boolean, editKey: Option[Key] = None)

import Action.*

object Main extends App {
  val persistTodos = (todos: List[TodoItem]) => {
    val item = JSON.stringify(
      todos
        .map(ti =>
          new TodoItemObject {
            override val completed: Boolean = ti.completed
            override val title: ByteString  = ti.title
          }
        )
        .toJSArray
    )
    window.localStorage.setItem("todos", item)
  }
  def pushPath     = (path: String) => {
    println(path)
    history.pushState({}, "", path)
  }

  val INIT_TODOS      = {
    val ts = window.localStorage.getItem("todos")
    if (ts == null) List.empty[TodoItem]
    else {
      JSON
        .parse(ts)
        .asInstanceOf[js.Array[TodoItemObject]]
        .map(o => TodoItem(createKey(), o.title, o.completed))
        .toList
    }
  }
  val popstateFilters =
    windowEvents(_.onPopState)
      .map(_ => window.location.pathname)
      .map(path =>
        FILTERS
          .find(_.path == path)
          .getOrElse(FILTERS.head)
      )

  val actionBus    = new EventBus[Action]
  val filterBus    = new EventBus[Filter]
  var state        = Var(State(INIT_TODOS, false, None))
  val stateUpdater = state.updater[Action] { case (state: State, action: Action) =>
    val State(todos, toggleAll, editKey) = state
    val newTodos                         = action match {
      case Add(label)         => todos :+ TodoItem(createKey(), label, false)
      case Toggle(key)        => todos.map(t => if (t.key == key) t.copy(completed = !t.completed) else t)
      case Save(title)        =>
        editKey match {
          case Some(key) =>
            if (title == "")
              todos.filterNot(_.key == key)
            else
              todos.map(t => if (t.key == key) t.copy(title = title) else t)
          case None      => todos
        }
      case Destroy(key)       => todos.filterNot(_.key == key)
      case ToggleAll          => todos.map(_.copy(completed = !toggleAll))
      case ClearCompleted     => todos.filterNot(_.completed)
      case Edit(_) | ExitEdit => todos
    }
    val newToggleAll                     = action match {
      case ToggleAll => !toggleAll
      case _         => newTodos.nonEmpty && newTodos.forall(_.completed)
    }
    val newEditKey                       = action match {
      case Edit(k)  => Some(k)
      case ExitEdit => None
      case Save(_)  => None
      case _        => editKey
    }
    State(newTodos, newToggleAll, newEditKey)
  }

  val todosSignal  = state.signal.map(_.todos)
  val filterSignal = filterBus.stream.mergeWith(popstateFilters.toObservable).startWith(FILTERS.head)

  val app = sectionTag(
    todosSignal --> persistTodos,
    actionBus --> stateUpdater,
    filterBus.events.map(_.path) --> pushPath,
//    popstateFilters --> filterBus,
    cls("todoapp"),
    headerTag(
      cls("header"),
      h1("todos"),
      input(
        cls("new-todo"),
        autoFocus := true,
        placeholder("What needs to be done?"),
        onKeyUp.filter(_.keyCode == KeyCode.Enter).mapToValue.map(Add.apply).setValue("") --> actionBus.writer
      )
    ),
    child.maybe <-- todosSignal
      .map(_.nonEmpty)
      .map(
        Option.when(_)(
          sectionTag(
            cls("main"),
            input(
              cls("toggle-all"),
              idAttr("toggle-all"),
              typ("checkbox"),
              checked <-- state.signal.map(_.toggleAll),
              onClick.mapTo(ToggleAll) --> actionBus.writer
            ),
            label(forId("toggle-all"), "Mark all as completed"),
            ul(
              cls("todo-list"),
              children <-- todosSignal
                .combineWith(filterSignal)
                .map((todos, filter) => filter.filter(todos))
                .map(
                  _.map(todo =>
                    li(
                      cls("completed") := todo.completed,
                      cls("editing") <-- state.signal.map(_.editKey.exists(_ == todo.key)),
                      div(
                        cls("view"),
                        input(
                          cls("toggle"),
                          typ("checkbox"),
                          checked(todo.completed),
                          onClick.mapTo(Toggle(todo.key)) --> actionBus.writer
                        ),
                        label(onClick.mapTo(Edit(todo.key)) --> actionBus.writer, todo.title),
                        button(cls("destroy"), onClick.mapTo(Destroy(todo.key)) --> actionBus.writer)
                      ),
                      input(
                        cls("edit"),
                        defaultValue(todo.title),
                        onKeyUp.filter(_.keyCode == KeyCode.Enter).mapToValue.map(_.trim).map(Save.apply).setValue("") --> actionBus.writer,
                        onKeyUp.filter(_.keyCode == KeyCode.Escape).mapTo(ExitEdit).setValue("") --> actionBus.writer,
                        onBlur.mapToValue.map(_.trim).map(Save.apply).setValue("") --> actionBus.writer
                      )
                    )
                  )
                )
            )
          )
        )
      ),
    child.maybe <-- todosSignal
      .map(_.nonEmpty)
      .map(
        Option.when(_)(
          footerTag(
            cls("footer"),
            span(
              cls("todo-count"),
              strong(text <-- todosSignal.map(_.length.toString)),
              text <-- todosSignal.map(_.length > 1).map(v => if (v) " items left" else " item left")
            ),
            ul(
              cls("filters"),
              FILTERS.map(filter =>
                li(
                  a(
                    href(filter.path),
                    onClick.preventDefault.mapTo(filter) --> filterBus,
                    cls("selected") <-- filterSignal.map(_ == filter),
                    filter.label
                  )
                )
              )
            ),
            child.maybe <-- todosSignal
              .map(_.exists(_.completed))
              .map(
                Option.when(_)(button(cls("clear-completed"), onClick.mapTo(ClearCompleted) --> actionBus.writer, "Clear completed"))
              )
          )
        )
      )
  )
  render(document.querySelector("#app"), app)
}
