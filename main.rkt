#lang racketscript/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/stx
                     syntax/parse)
         racketscript/interop
         racket/stxparam
         racket/list)

(define React ($/require/* "react"))
(define ReactDOM ($/require/* "react-dom"))

(provide render
         <el
         <>
         $props
         define-component
         create-context
         with-context
         use-context
         in-context
         $ctx
         use-state
         define-state
         use-effect
         use-reducer
         with-reducer-context
         $state
         $action
         in-reducer-context
         $ctx-state
         $ctx-dispatch
         use-callback
         use-memo
         use-ref
         use-imperative-handle
         use-layout-effect
         use-debug-value)

;; Basic hooks
(define (use-state default-state)
    (apply values (js-array->list (#js.React.useState default-state))))

;; (define-state name val) is shorthand for creating a React State Hook
;; It defines two identifiers:
;; - name: the current value of the state
;; - set-name!: a setter used to change the state's value
(define-syntax define-state
  (syntax-parser
    [(_ name default-val)
     #:with set-name! (format-id #'name "set-~a!" #'name)
     #'(define-values (name set-name!) (use-state default-val))]))

(define use-effect #js.React.useEffect)

(define use-context #js.React.useContext)

;; Additional hooks
(define (use-reducer reducer initial-state [init $/undefined])
    (apply values (js-array->list (#js.React.useReducer reducer initial-state init))))

(define use-callback #js.React.useCallback)

(define use-memo #js.React.useMemo)

(define use-ref #js.React.useRef)

(define use-imperative-handle #js.React.useImperativeHandle)

(define use-layout-effect #js.React.useLayoutEffect)

(define use-debug-value #js.React.useDebugValue)

;; Basic API
(define create-context #js.React.createContext)

(define create-element 
    (lambda (component #:props [props null] . children)
        (apply #js.React.createElement
            (append
                (list (racket->js component) props)
                (map racket->js (flatten children))))))

(define (racket->js node)
    (cond
        [(or (string? node) (number? node)) ($/str node)]
        [else node]))

;; A small alias for readability
(define <el create-element)

;; a macro version of <el that tries to hide some boilerplate
(define-syntax <>
  (syntax-parser
    [(_ name #:props ([x:id v] ...) . body)
     #'(<el name #:props ($/obj [x v] ...) . body)]
    [(_ name . body) #'(<el name . body)]))

(define-syntax-parameter $props
  (syntax-parser
    [_ #'(error '$props "Warning: $props keyword cannot be used outside Rackt define-component body")]))

(define-syntax define-component
  (syntax-parser
    [(_ name . body)
     #'(define (name props . ..)
         (syntax-parameterize
             ;; $props may be used as an id, 
             ;; or in head position where an implicit $ is inserted
             ([$props (syntax-parser
                        [:id #'props]
                        [(_ . args) #'($ props . args)])])
           . body))]))

(define-syntax with-context
  (syntax-parser
    [(_ ctx-name (~datum =) default-value . body)
     #'(<> ($ ctx-name 'Provider) #:props ([value default-value]) . body)]))

(define-syntax-parameter $ctx
  (syntax-parser
    [_ #'(error '$ctx "Warning: $ctx keyword cannot be used outside Rackt in-context body")]))

(define-syntax in-context
  (syntax-parser
    [(_ ctx-name . body)
     #'(let ([ctx (use-context ctx-name)])
         (syntax-parameterize
             ;; $ctx may be used as an id,
             ;; or in head position where an implicit $ is inserted
             ([$ctx (syntax-parser
                      [:id #'ctx]
                      [(_ . args) #'($ ctx . args)])])
           . body))]))

(define-syntax-parameter $state
  (syntax-parser
    [_ #'(error '$state "Warning: $state keyword cannot be used outside Rackt with-reducer-context action table")]))

(define-syntax-parameter $action
  (syntax-parser
    [_ #'(error '$action "Warning: $action keyword cannot be used outside Rackt with-reducer-context action table")]))

(begin-for-syntax
  (define (id->str id)
    (datum->syntax id (symbol->string (syntax->datum id)))))

;; like with-context, but the given context must be a Reducer,
;; ie, it only has two properties corresponding the Reducer "state" and "dispatch"
(define-syntax with-reducer-context
  (syntax-parser
    [(_ ctx-name #:init init-state #:actions ([action-type:id e] ...) . body)
     #:with (action-str ...) (stx-map id->str #'(action-type ...))
     #`(begin
         (define-values (_store _dispatch)
           (use-reducer
            (lambda (state action)
              (syntax-parameterize
                  ;; $state and $action may be used as ids,
                  ;; or in head position where an implicit $ is inserted
                  ([$state (syntax-parser
                             [:id #'state]
                             [(_ . args) #'($ state . args)])]
                   [$action (syntax-parser
                              [:id #'action]
                              [(_ . args) #'($ action . args)])])
                (cond [(eq? ($ action 'type) action-str) e] ... [else state])))
            init-state))
         (with-context ctx-name = ($/obj [store _store] [dispatch _dispatch]) . body))]))

(define-syntax-parameter $ctx-state
  (syntax-parser
    [_ #'(error '$ctx-state "Warning: $ctx-state keyword cannot be used outside Rackt in-reducer-context body")]))

(define-syntax-parameter $ctx-dispatch
  (syntax-parser
    [_ #'(error '$ctx-dispatch "Warning: $ctx-dispatch keyword cannot be used outside Rackt in-reducer-context body")]))

;; like in-context, but the given context must be a Reducer,
;; ie, it only has two properties corresponding the Reducer "state" and "dispatch".
;; There are implicitly bound to $ctx-state and $ctx-dispatch, respectively
(define-syntax in-reducer-context
  (syntax-parser
    [(_ ctx-name . body)
     #'(begin
         (in-context ctx-name
           (let ([store ($ctx 'store)]
                 [dispatch ($ctx 'dispatch)])
             (syntax-parameterize
                 ;; $store and $dispatch may be used as ids,
                 ;; or in head position where an implicit $ is inserted
                 ([$ctx-state (syntax-parser
                                [:id #'store]
                                [(_ . args) #'($ store . args)])]
                  [$ctx-dispatch
                   (syntax-parser
                     [:id #'dispatch]
                     [(_ obj) #'(dispatch obj)] ; given explicit obj
                     [(_ action-type:id . props+vals)
                      #`(dispatch ($/obj [type #,(id->str #'action-type)] . props+vals))])])
               . body))))]))

(define (render react-element node-id)
    (#js.ReactDOM.render react-element (#js*.document.getElementById node-id)))
