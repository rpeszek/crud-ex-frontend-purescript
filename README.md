Polymorphic CRUD implemented in PureScript.  
See my CRUD umbrella project:  [typesafe-web-polyglot](https://github.com/rpeszek/typesafe-web-polyglot.git).

### Goals:  
* Comprehensive single page app (CRUD) example using [halogen](https://pursuit.purescript.org/packages/purescript-halogen) and hash [routing](https://github.com/slamdata/purescript-routing)
* CRUD polymorphism
* Play with PureScript extensible effects (effect rows)

### Backends:  
This work has not been integrated into my backend projects yet, needs to be run standalone
by opening provided index.html.

### Notes:  
I made conscious decision to stay away from transformers to force myself to use row effects more (e.g. (appconf:: APPCONFIG | eff) instead of ReaderT/MonadAsk) 
I even keep Either explicitly typed.

_I love PureScript!_  Type safety is great and the overall experience was quite smooth.

### Places where I got in trouble:  
I wanted to do something like this (keep effects fully polymorphic instead of hardcoding Aff):  
```PureScript
class Monad m <= EntityGetM m model where
  getEntity :: KeyT model -> m model
  ... 
```
Expected easy garden path walk from here, instead it felt like swimming upstream.  
Current version of PureScript will not let implement instances of row effects, 
so this is a no go:
```PureScript
instance serverGet ::  EntityGetM (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff)) Thing where ...
```
I can try to use newtype and re-derive everything, but then I am not taking advantage of effect rows.  
I am looking forward to next version of PureScript 
(if something like this becomes available, see ): 
```PureScript
instance serverGet :: (eff ~ (ajax :: AJAX, appconf:: APPCONFIG)) => EntityGetM eff Thing where ...
```
So, for now, my type classes hardcode Aff:
```PureScript
type ServerM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff))
type ServerErrM eff a = ServerM eff (Either String a)

class EntityGET e model where
  getEntity :: KeyT model -> ServerErrM e model 
  ...
``` 

I also expected lifting from fewer to more effects to be trivial.  
It seems to not work well with use of type classes that hardcode different sets of row effects, std use of liftEff/liftAff will not type check.  
So I ended up doing something UGLY like this:
```PureScript
liftAddNav :: forall e a . Aff e a -> Aff (nav :: NAVIGATION | e) a
liftAddNav = unsafeCoerceAff  
```
This may mean that I do not know PureScript well enough. It had been only about 2 weeks. 
