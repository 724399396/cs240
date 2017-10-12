/* this version :
   can't represent exceptions, thunk
   gc can't kown args length
   always need a pointer
*/
struct Val {
  unsigned long constrono; /* constructor # */
  struct Val *args[];      /* flexible array */
};

/* ================================================================ */
typedef struct Val {
  const struct ValInfo *info;
  struct Val *args[];
} Val;

/* Statically allocated at compile time. Only one per
   constructor (or closure-createing expression, etc.) */
struct ValInfo {
  struct GCInfo gcInfo; /* for garbage collector */
  enum { CONSTRNO, FUNC, THUNK, IND } tag;
  union {
    unsigned int constrno;
    Val *(*func) (const Val *closure, const Val *arg);
    Exception *(*thunk) (Val *closure);
  };
};

/* currying */
const3 :: a -> b -> c -> a
const3 a b c = a;

Val *const3_1 (Val *ignored, Val *a)
{
  v = (Val *) gc_malloc (offsetof (Val, args[1]));
  v->info = &const3_2_info;
  v->args[0] = a;
  return v;
}

Val *const3_2 (Val *closure, Val *b)
{
  v = (Val *) gc_malloc (offsetof (Val, args[2]));
  v->info = &const3_3_info;
  v->args[0] = b;
  v->args[1] = closure;
  return v;
}

Val *const3_3 (Val *v, Val *c)
{
  return v->args[1]->args[0];
}

/* fix int to pointer overhead */
union Arg {
  struct Val *boxed;
  unsigned long unboxed;
};

typedef struct Val {
  const struct ValInfo *info;
  union Arg args[];
};

/* seq implement */
const struct ValInfo seq_info = {
  some_gcinfo, THUNK, .thunk = &seq_thunk
};

Val *seq_2 (Val *closure, Val *b)
{ /* assume seq_1 put first arg of (seq a b) in closure */
  c = (Val *) gc_malloc (offsetof (Val, args[2]));
  c->info = &seq_Info;
  c->args[0] = closure-args[0];
  c->args[1] = b;
  return c;
}

Exception *seq_thunk(Void *c)
{
  Exception *e = force (&c->args[0]);
  if (!e) {
    c->info = &ind_info;
    c->args[0] = c->args[1];
  }
  return 1;
}
