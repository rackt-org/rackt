#lang racketscript/base

(require (for-syntax racket/base syntax/parse)
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
         use-state
         use-effect
         use-context
         use-reducer
         use-callback
         use-memo
         use-ref
         use-imperative-handle
         use-layout-effect
         use-debug-value)

;; Basic hooks
(define (use-state default-state)
    (apply values (js-array->list (#js.React.useState default-state))))

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

(define (render react-element node-id)
    (#js.ReactDOM.render react-element (#js*.document.getElementById node-id)))
