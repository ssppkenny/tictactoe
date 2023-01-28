// This line opens the Tea.App modules into the current scope for Program access functions and types
//
open Tea
open Tea.App
open Tea_html.Attributes
open Tea_html.Events
open Tea.Html
open Tea.Mouse
open Tea.App

// This opens the Elm-style virtual-dom functions and types into the current scope
open Belt
// Let's create a new type here to be our main message type that is passed around
type msg = Coords(int, int) // This will be our message to increment the counter

// the model for Counter is just an integer
type state = list<list<(string, int)>>
type model = {
  state: list<list<(string, int)>>,
  move: int,
  winnerCoords: array<(int, int)>,
}

// This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values
let init = () => {
  state: list{
    list{("left_upper", 0), ("center_upper", 0), ("right_upper", 0)},
    list{("left_middle", 0), ("center_middle", 1), ("right_middle", 0)},
    list{("left_lower", 0), ("center_lower", 0), ("right_lower", 0)},
  },
  move: 1,
  winnerCoords: [],
}

let toCheck = [
  [0, 1, 2],
  [3, 4, 5],
  [6, 7, 8],
  [0, 3, 6],
  [1, 4, 7],
  [2, 5, 8],
  [2, 4, 6],
  [0, 4, 8],
]

let coords = [
  [(0, 0), (0, 1), (0, 2)],
  [(1, 0), (1, 1), (1, 2)],
  [(2, 0), (2, 1), (2, 2)],
  [(0, 0), (1, 0), (2, 0)],
  [(0, 1), (1, 1), (2, 1)],
  [(0, 2), (1, 2), (2, 2)],
  [(0, 2), (1, 1), (2, 0)],
  [(0, 0), (1, 1), (2, 2)],
]

let freePos = (s: state) => {
  let fp = []
  s->List.forEachWithIndex((i, x) => {
    x->List.forEachWithIndex((j, y) => {
      let (_, v) = y
      if v == 0 {
        Js.Array.push((i, j), fp)->ignore
      }
    })
  })
  fp
}

let areSame = (r: array<int>) => {
  let a = r->Array.getUnsafe(0)
  let b = r->Array.getUnsafe(1)
  let c = r->Array.getUnsafe(2)
  a == b && b == c && a == c && a > 0
}

let check = (m: model) => {
  let s = m.state
  let row = []
  s->List.forEachWithIndex((_, x) => {
    x->List.forEachWithIndex((_, y) => {
      let (_, v) = y
      Js.Array.push(v, row)->ignore
    })
  })

  let a = toCheck->Array.map(x => {
    let e1 = x->Array.getUnsafe(0)
    let e2 = x->Array.getUnsafe(1)
    let e3 = x->Array.getUnsafe(2)
    let retVal = areSame([
      row->Array.getUnsafe(e1),
      row->Array.getUnsafe(e2),
      row->Array.getUnsafe(e3),
    ])
    retVal
  })

  let opt = a->Array.getIndexBy(x => x)
  let ind = opt->Option.getWithDefault(-1)
  let retVal = if ind >= 0 {
    coords[ind]
  } else {
    None
  }
  retVal
}

let nextCoords = (m: model, fp: array<(int, int)>) => {
  let l = Array.length(fp)
  if l == 0 {
    None
  } else {
    let r = Js.Math.random_int(0, l)
    fp->Array.get(r)
  }
}
let nextMove = (m: model) => {
  if m.move == 1 {
    2
  } else {
    1
  }
}

// This is the central message handler, it takes the model as the first argument
let update = (model: model, msg: msg) => {
  switch msg {
  | Coords(i, j) => {
      let s = model.state->List.mapWithIndex((n, x) =>
        x->List.mapWithIndex((m, y) => {
          let (a, v) = y
          let p = nextMove(model)
          if n == i && m == j {
            (a, p)
          } else {
            (a, v)
          }
        })
      )
      let nm = nextMove(model)
      let newModel = {...model, state: s, move: nm}
      let opt = check(newModel)
      let sameCoords = opt->Option.getWithDefault([])

      //

      switch nm {
      | 1 => ({...newModel, winnerCoords: sameCoords}, Cmd.none)
      | 2 =>
        let fp = freePos(s)
        let nc = nextCoords(newModel, fp)
        if nc->Option.isNone {
          ({...newModel, winnerCoords: sameCoords}, Cmd.none)
        } else if Array.length(sameCoords) > 0 {
          ({...newModel, winnerCoords: sameCoords}, Cmd.none)
        } else {
          let (x, y) = nc->Option.getWithDefault((-1, -1))
          ({...newModel, winnerCoords: sameCoords}, Cmd.msg(Coords(x, y)))
        }
      | _ => ({...newModel, winnerCoords: sameCoords}, Cmd.none)
      }
    }
  }
}

// This is just a helper function for the view, a simple function that returns a button based on some argument
let viewButton = (title: string, msg: msg) => button(list{Events.onClick(msg)}, list{text(title)})

let subscriptions = model => {
  Sub.none
}
let cellStyle = (i: int, j: int, m: model) => {
  let wc = m.winnerCoords
  if Js.Array.findIndex(x => {x == (i, j)}, wc) >= 0 {
    let retVal = list{Attributes.classList(list{("circle", true), ("winner", true)})}
    retVal
  } else {
    let retVal = list{Attributes.class("circle")}
    retVal
  }
}
// This is the main callback to generate the virtual-dom.
// This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing
let view = (model: model): Vdom.t<msg> => {
  let retVal = div(
    list{Attributes.class("wrapper")},
    list{
      div(
        list{Attributes.class("field")},
        model.state->List.mapWithIndex((i, x) =>
          div(
            list{Attributes.class("row")},
            x->List.mapWithIndex((j, y) => {
              let (s, v) = y
              let t = if v == 0 {
                ""
              } else if v == 1 {
                "x"
              } else {
                "o"
              }
              div(
                list{Attributes.class(s), Events.onClick(Coords(i, j))},
                list{div(cellStyle(i, j, model), list{text(t)})},
              )
            }),
          )
        ),
      ),
    },
  )
  retVal
}

// This is the main function, it can be named anything you want but `main` is
// traditional.  The Program returned here has a set of callbacks that can easily be
// called from Rescript or from javascript for running this main attached to an
// element, or even to pass a message into the event loop.  You can even expose the
// constructors to the messages to javascript via the above [@@bs.deriving
// {accessors}] attribute on the `msg` type or manually, that way even javascript can
// use it safely.
let main = standardProgram({
  init: () => (init(), Tea_cmd.none),
  update,
  view,
  subscriptions,
})
