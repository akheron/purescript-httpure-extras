module TodoExample (main) where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import HTTPure as HTTPure
import HTTPure.Rest as Rest
import HTTPure.Router as Router


-- TODO item

newtype Todo = Todo { id :: Int, done :: Boolean, description :: String }

derive instance genericTodo :: Generic Todo _

derive instance newtypeTodo :: Newtype Todo _

derive newtype instance showTodo :: Show Todo

instance encodeTodo :: Encode Todo where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })


-- Request body for creating/updating a TODO item

newtype TodoBody = TodoBody { done :: Boolean, description :: String }

derive instance genericTodoBody :: Generic TodoBody _

instance decodeTodoBody :: Decode TodoBody where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })


-- Example data

todo1 :: Todo
todo1 = Todo { id: 1, done: false, description: "An example todo item" }

todo2 :: Todo
todo2 = Todo { id: 2, done: true, description: "Another example" }

exampleTodos :: Array Todo
exampleTodos = [ todo1, todo2 ]


-- Helper for finding todos

matchId :: Int -> Todo -> Boolean
matchId id (Todo todo) = id == todo.id

findTodo :: Int -> Array Todo -> Maybe Todo
findTodo id = Array.find (matchId id)

-- List, create, read, update and delete request handlers for the TODO resource

listTodos :: Rest.Request Unit Unit -> Aff HTTPure.Response
listTodos request = Rest.okJSON exampleTodos

createTodo :: Rest.Request Unit TodoBody -> Aff HTTPure.Response
createTodo request =
  Rest.okJSON $ makeTodo request.body
  where
    makeTodo (TodoBody { done, description }) =
      Todo { id: 42, done, description }

readTodo :: Rest.Request Int Unit -> Aff HTTPure.Response
readTodo request =
  case findTodo request.id exampleTodos of
    Nothing -> HTTPure.notFound
    Just todo -> Rest.okJSON todo

updateTodo :: Rest.Request Int TodoBody -> Aff HTTPure.Response
updateTodo request =
  case findTodo request.id exampleTodos of
    Nothing -> HTTPure.notFound
    Just todo ->
      let newTodo = modify todo request.body
      in do
        liftEffect $ Console.log $
          "Would update TODO " <> show request.id <> " as " <> show newTodo
        Rest.okJSON newTodo
  where
    modify (Todo todo) (TodoBody body) =
      Todo $ todo { done = body.done, description = body.description }

deleteTodo :: Rest.Request Int Unit -> Aff HTTPure.Response
deleteTodo request =
  case findTodo request.id exampleTodos of
    Nothing -> HTTPure.notFound
    Just todo -> do
      liftEffect $ Console.log ("Would delete " <> show todo)
      HTTPure.noContent

routes :: Array Router.Route
routes =
  [ Rest.route ["todo"] $
      Rest.endpoint Int.fromString
        # Rest.list listTodos
        # Rest.create createTodo
        # Rest.read readTodo
        # Rest.update updateTodo
        # Rest.delete deleteTodo
  ]

main :: Effect Unit
main =
  HTTPure.serve 8080 (Router.router routes) do
    Console.log "Listening on port 8080"
