open Diffract

(* Example 1: Nested rules - find deprecated API calls inside specific classes *)
let example_nested_rules () =
  print_endline "=== Example 1: Nested Rules ===";
  print_endline "Find println() calls only inside classes annotated with @Service\n";

  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@

@Service
class $CLASS { $BODY }

@@
match: strict
metavar $MSG: single
@@
println($MSG)|} in

  let source_text = {|
@Service
class UserService {
    fun createUser(name: String) {
        println("Creating user: $name")
        // business logic
    }

    fun deleteUser(id: Int) {
        println("Deleting user: $id")
    }
}

class Utils {
    companion object {
        fun log(msg: String) {
            println("LOG: $msg")
        }
    }
}

fun standalone() {
    println("This is outside any class")
}
|} in

  let results = Match.find_nested_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in

  Printf.printf "Found %d println() calls inside classes:\n\n" (List.length results);
  List.iter (fun (r : Match.nested_match_result) ->
    let class_name = List.assoc "$CLASS" (List.hd r.contexts).context_bindings in
    let msg = List.assoc "$MSG" r.inner_bindings in
    Printf.printf "  In class '%s' at line %d: println(%s)\n"
      class_name
      (r.start_point.row + 1)
      msg
  ) results;
  print_newline ()


(* Example 2: Nested rules - find methods inside classes *)
let example_methods_in_classes () =
  print_endline "=== Example 2: Nested Rules - Methods Inside Classes ===";
  print_endline "Find all methods defined inside classes\n";

  (* Two-level nesting: class -> function *)
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
class $CLASS { $BODY }

@@
match: strict
metavar $METHOD: single
metavar $MBODY: sequence
@@
fun $METHOD() { $MBODY }|} in

  let source_text = {|
class UserService {
    fun createUser() {
        println("Creating user")
    }

    fun deleteUser() {
        println("Deleting user")
    }
}

class ProductService {
    fun listProducts() {
        database.query("SELECT * FROM products")
    }
}

fun standaloneFunction() {
    println("I'm not in a class")
}
|} in

  let results = Match.find_nested_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in

  Printf.printf "Found %d methods inside classes (standalone function excluded):\n\n" (List.length results);
  List.iter (fun (r : Match.nested_match_result) ->
    let class_name = List.assoc "$CLASS" (List.hd r.contexts).context_bindings in
    let method_name = List.assoc "$METHOD" r.inner_bindings in
    Printf.printf "  %s.%s() at line %d\n"
      class_name
      method_name
      (r.start_point.row + 1)
  ) results;
  print_newline ()


(* Example 3: Partial matching - find function calls with specific named arguments *)
let example_partial_matching () =
  print_endline "=== Example 3: Partial Matching ===";
  print_endline "Find function calls that include 'timeout' as a named argument\n";

  (* In Kotlin, named arguments look like: func(timeout = 5000, retries = 3)
     We use partial matching to find calls with 'timeout' regardless of other args *)
  let pattern_text = {|@@
match: partial
metavar $FUNC: single
metavar $VALUE: single
@@
$FUNC(timeout = $VALUE)|} in

  let source_text = {|
fun main() {
    // Various API calls with different arguments
    fetchData(url = "http://api.example.com", timeout = 5000)
    fetchData(timeout = 3000, retries = 3)
    sendRequest(body = "data", timeout = 10000, headers = mapOf())
    quickCall(url = "http://fast.api")  // no timeout
    connect(host = "localhost", port = 8080, timeout = 1000)
}
|} in

  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in

  Printf.printf "Found %d calls with 'timeout' argument:\n\n" (List.length results);
  List.iter (fun (r : Match.match_result) ->
    let func = List.assoc "$FUNC" r.bindings in
    let timeout = List.assoc "$VALUE" r.bindings in
    Printf.printf "  Line %d: %s(..., timeout = %s, ...)\n"
      (r.start_point.row + 1)
      func
      timeout
  ) results;
  print_newline ()


(* Example 4: Combining nested rules with partial matching *)
let example_partial_nested () =
  print_endline "=== Example 4: Partial Matching in Nested Context ===";
  print_endline "Find classes that have methods containing 'return' statements\n";

  (* First find classes, then find return statements inside them *)
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
class $CLASS { $BODY }

@@
match: strict
metavar $EXPR: single
@@
return $EXPR|} in

  let source_text = {|
class Calculator {
    fun add(a: Int, b: Int) {
        return a + b
    }

    fun multiply(a: Int, b: Int) {
        return a * b
    }

    fun printResult(x: Int) {
        println(x)
    }
}

class Logger {
    fun log(msg: String) {
        println(msg)
    }
}
|} in

  let results = Match.find_nested_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in

  Printf.printf "Found %d return statements inside classes:\n\n" (List.length results);
  List.iter (fun (r : Match.nested_match_result) ->
    let class_name = List.assoc "$CLASS" (List.hd r.contexts).context_bindings in
    let expr = List.assoc "$EXPR" r.inner_bindings in
    Printf.printf "  In %s: return %s (line %d)\n"
      class_name
      expr
      (r.start_point.row + 1)
  ) results;
  print_newline ()


(* Example 5: Three-level nesting - find specific calls in methods in classes *)
let example_deep_nesting () =
  print_endline "=== Example 5: Deep Nesting (3 levels) ===";
  print_endline "Find database.query() calls inside methods inside Repository classes\n";

  let pattern_text = {|@@
match: strict
metavar $REPO: single
metavar $BODY1: sequence
@@
class $REPO { $BODY1 }

@@
match: strict
metavar $METHOD: single
metavar $BODY2: sequence
@@
fun $METHOD() { $BODY2 }

@@
match: strict
metavar $QUERY: single
@@
database.query($QUERY)|} in

  let source_text = {|
class UserRepository {
    fun findById() {
        database.query("SELECT * FROM users WHERE id = ?")
    }

    fun findAll() {
        database.query("SELECT * FROM users")
    }

    fun clearCache() {
        cache.clear()
    }
}

class ProductRepository {
    fun search() {
        database.query("SELECT * FROM products")
    }
}

fun standaloneQuery() {
    database.query("SELECT 1")
}
|} in

  let results = Match.find_nested_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in

  Printf.printf "Found %d database queries in Repository class methods:\n\n"
    (List.length results);
  Printf.printf "(Standalone query excluded because it's not inside a class)\n\n";
  List.iter (fun (r : Match.nested_match_result) ->
    let repo = List.assoc "$REPO" (List.nth r.contexts 0).context_bindings in
    let method_name = List.assoc "$METHOD" (List.nth r.contexts 1).context_bindings in
    let query = List.assoc "$QUERY" r.inner_bindings in
    Printf.printf "  %s.%s(): %s\n" repo method_name query
  ) results;
  print_newline ()


let () =
  example_nested_rules ();
  example_methods_in_classes ();
  example_partial_matching ();
  example_partial_nested ();
  example_deep_nesting ()
