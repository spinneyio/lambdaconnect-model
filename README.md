# lambdaconnect-model

A Clojure library designed to parse CoreData model and transform it into Datomic schema. <br> <br>
[![Build Status](https://app.travis-ci.com/spinneyio/lambdaconnect-model.svg?branch=master)](https://app.travis-ci.com/spinneyio/lambdaconnect-model)

## Installation

Leiningen coordinates:
```clojure
[io.spinney/lambdaconnect-model "1.0.1"]
```

## Usage

To read and parse CoreData model use `entities-by-name` function and pass path to `xml` model.

```
(require '[lambdaconnect-model.core :as tmp])
(def entities-by-name (tmp/entities-by-name "resources/model.xml"))
```

To get Datomic schema use `datomic-schema` function:

```
(def datomic-schema (tmp/datomic-schema entities-by-name))
```

To generate spec use `specs` function:

```
(tmp/specs entities-by-name)
```

Use generated specs:

```
(require '[clojure.spec.alpha :as s])
(def entity {:LAGame/skillLevelRequired "beginner",
             :app/updatedAt #inst "2018-06-28T04:40:22.111-00:00",
             :LAGame/sport #:app{:uuid #uuid "55550e26-72e7-4172-8f46-d6a724e5476c"},
             :LAGame/date #inst "2018-06-28T04:40:22.111-00:00",
             :LAGame/location #:app{:uuid #uuid "33330e26-72e7-4172-8f46-d6a724e5476c"},
             :app/createdAt #inst "2018-06-28T04:40:22.111-00:00",
             :LAGame/inThePast false,
             :app/active true,
             :LAGame/organiser #:app{:uuid #uuid "44440e26-72e7-4172-8f46-d6a724e5476c"},
             :LAGame/teamName "Bulls",
             :LAGame/gameDescription "My cool game",
             :LAGame/title "Awesome game",
             :app/uuid #uuid "20840e26-72e7-4172-8f46-d6a724e5476c"})
(s/explain-str (tmp/spec-for-name :LAGame) entity)
```

Define scoping:

```
(def scoping (tmp/read-pull-scoping-edn "resources/scope.edn" entities-by-name))
```

## License

Copyright © 2022 Spinney

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.