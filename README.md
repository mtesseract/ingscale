This is Ingscale, a library and a command line tool for convenient
scaling of ingredients lists.

Here is an example:

    $ cat example.txt
    Servings, 4
    Soy Milk, 3 cups
    Salt, 1/2 tsp
    Brown Sugar, 3 1/4 tbsp
    Foo Flour, 200g
    Roasted Bar, 0.6 l
    $

We can scale this ingredients list by a fixed factor, e.g.:

    $ ingscale --scale-by 2 < example.txt
    Servings, 8 
    Soy Milk, 6 cup [1.42 l, 1420 ml, 288 tsp, 96 tbsp, 48 fl.oz]
    Salt, 1 tsp [0.005 l, 5 ml, 0.02 cup, 1/3 tbsp, 0.17 fl.oz]
    Brown Sugar, 6 1/2 tbsp [0.096 l, 96 ml, 0.41 cup, 19 1/2 tsp, 3 1/4 fl.oz]
    Foo Flour, 400 g [14 oz]
    Roasted Bar, 1.2 l [1200 ml, 5 cup, 243 1/2 tsp, 81 1/4 tbsp, 40 1/2 fl.oz]
    $

Or we can scale it to a fixed ingredient quantity, e.g.:

    $ ingscale --scale-to "Soy Milk, 2.5 cup" < example.txt
    Servings, 3 1/3 
    Soy Milk, 2 1/2 cup [0.591 l, 591 ml, 120 tsp, 40 tbsp, 20 fl.oz]
    Salt, 0.42 tsp [0.002 l, 2 ml, 0.01 cup, 0.14 tbsp, 0.07 fl.oz]
    Brown Sugar, 2.71 tbsp [0.04 l, 40 ml, 0.17 cup, 8 1/4 tsp, 1 1/3 fl.oz]
    Foo Flour, 166 2/3 g [6 oz]
    Roasted Bar, 1/2 l [500 ml, 2.11 cup, 101 1/2 tsp, 33 3/4 tbsp, 17 fl.oz]
    $

That's about it.

The code can be built with stack, that is, "stack build" should be
sufficient to build it.
