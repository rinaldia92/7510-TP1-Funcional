(ns logical-interpreter)
(require '[clojure.string :as str])

;------------------------------general----------------------------------------

(defn database-to-vec "Leo database y devuelvo lista" [archivo]
	(str/split (str/replace (str/replace (str/replace archivo #"\." "") #" " "") #"\t" "")  #"\n"))

(defn fnaux [line]
	(or (re-find #".*\(.*\)\." line) (re-find #".*\(.*\) :- .*\(.*\)\." line)))

(defn check-database "Chequeo si la database tiene formato correcto"[database]
	(let [split (filter #(not (str/blank? %)) (str/split database  #"\n"))]
	  (not (contains? (into #{} (map #(fnaux %) split)) nil))))


(defn is-fact [line]
    (if (not (re-find #":-" line)) line))

(defn facts-set "Filtro de la lista por facts" [list]
	(filter identity (map is-fact list)))

(defn facts-parser "Parseo los facts" [line]
  	(str/split (str/replace (str/replace (str/replace line #" " "")#"\(" " ") #"\)" " ") #" "))

(defn fact-map "Creo un diccionario en base a un fact" [fact]
	(if (not= fact nil) (assoc {} (nth fact 0) [(str/split (nth fact 1) #",")])))

(defn facts-process "Creo un diccionario con todos los facts, key: facts value: [arguments]" [factslist]
	(reduce #(merge-with into %1 %2) (map fact-map (drop 1 (map facts-parser factslist)))))


(defn is-rule [line]
	(if (re-find #":-" line) line))

(defn rule-set "Filtro de la lista por reglas" [list]
	(filter identity (map is-rule list)))

(defn rule-name "Obtengo nombre de la rule" [rule]
	(str/replace rule #"\(.*\)" ""))

(defn rule-parameters "Obtengo parametros de la rule" [rule]
	(str/split (str/replace (str/replace rule #"\).*" "") #"^.*\(" "") #","))

(defn rule-map "Creo un map en base a una rule" [rule]
	(if (not= rule nil) (assoc {} (rule-name rule) [(rule-parameters rule) rule])))

(defn rules-process "Creo un map con todas las rules" [rulelist]
	(reduce #(merge-with into %1 %2) (map rule-map rulelist)))

(defn parser-rule-to-facts "Parseo los facts de la rule"[rule]
	(str/split (str/replace (str/replace rule #".*:-" "") #"\)," ") ") #" "))


(defn query-process "Preproceso la query" [query]
	(str/replace (str/replace query #"\.$" "") #" " ""))

(defn query-name "Obtengo el nombre de la query" [query]
	(str/replace query #"\(.*$" ""))

(defn query-parameters "Obtengo parametros de la query" [query]
	(str/split (str/replace (str/replace query #"\).*" "") #"^.*\(" "") #","))

(defn check-fact "Chequeo si cumple fact" [facts query-name query-parameters]
	(let [facts-arguments  (get facts query-name)]
		(contains? (into #{} facts-arguments) query-parameters)))

(defn make-tuple "Genero tuplas de parametros genericos y su reemplazo"
	[rule-parameters query-parameters]
	(for [x (range 0 (alength (into-array rule-parameters)) )]
	(list (nth rule-parameters x) (nth query-parameters x))))

(defn replace-in-rule "Reemplaza un parametro generico por parametro de query"
	[rule rule-parameter query-parameter]
	(str/replace rule rule-parameter query-parameter))

(defn generate-rule-to-process "Genera la rule completa para procesar"
	[query-parameters rule-parameters rule]
	(reduce #(replace-in-rule %1 (nth %2 0) (nth %2 1)) rule (make-tuple rule-parameters query-parameters)))

(defn check-facts "Chequea si las facts de las rules existen" [facts rules-facts]
	(let [facts-of-rules (map facts-parser rules-facts)]
		 (map #(check-fact facts (nth % 0) (str/split (nth % 1) #",")) facts-of-rules)))

(defn check-rule "Chequeo si cumple rule" [rules facts query-name query-parameters]
	(let [rule-parameters (nth (get rules query-name) 0)
		  rule (nth (get rules query-name) 1)
		  rulecomplete (generate-rule-to-process query-parameters rule-parameters rule)
		  facts-of-rules (parser-rule-to-facts rulecomplete)
		  checked-facts (check-facts facts facts-of-rules)]
		  (every? true? (into [] checked-facts))))

(defn parser-query "Parseo query para chequear algun match" [query facts rules]
	(let [ query-name (query-name query)
		   query-parameters (query-parameters query)]
		   (if (contains? facts query-name) (check-fact facts query-name query-parameters)
		   (if (contains? rules query-name) (check-rule rules facts query-name query-parameters)
		   false ))))

(defn check-query "Termina de procesar la database y evalua la query" [query factslist ruleslist]
	(let [facts (facts-process factslist)
		  rules (rules-process ruleslist)]
		  (parser-query query facts rules)))

(defn query-validate "Valida si la query tiene formato correcto"[query]
  (if (re-find #".*\(.*\)" query) true))

(defn process-and-evaluate "Procesa database y evalua la query"[database query]
	(let [list (database-to-vec database)
		  factslist (facts-set list)
		  ruleslist (rule-set list)
		  query-processed (query-process query)
		  ]
		  (if (check-database database) (check-query query-processed factslist ruleslist) nil)))

(defn evaluate-query "Si la query es correcta procesa la database y evalua la query"
	[database query]
	(if (query-validate query) (process-and-evaluate database query) nil))
