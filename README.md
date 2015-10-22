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

    $ cat example.txt | ./ingscale --scale-by 2
    Servings, 8 
    Soy Milk, 6 cup [1 1/2 l, 1420 ml, 288 tsp, 96 tbsp, 48 fl.oz]
    Salt, 2 tsp [1.0e-2 l, 10 ml, 4.0e-2 cup, 2/3 tbsp, 1/3 fl.oz]
    Brown Sugar, 6 tbsp [8.9e-2 l, 89 ml, 1/3 cup, 18 tsp, 3 fl.oz]
    Foo Flour, 400 g [14 oz]
    Roasted Bar, 1.2 l [1200 ml, 5 cup, 243 1/2 tsp, 81 1/4 tbsp, 40 1/2 fl.oz]
    $

Or we can scale it to a fixed ingredient quantity, e.g.:

    $ cat example.txt | ./ingscale --scale-to "Soy Milk, 2.5 cup"
    Servings, 3 1/3 
    Soy Milk, 2 1/2 cup [2/3 l, 591 ml, 120 tsp, 40 tbsp, 20 fl.oz]
    Salt, 0.83 tsp [4.0e-3 l, 4 ml, 2.0e-2 cup, 1/4 tbsp, 1/4 fl.oz]
    Brown Sugar, 2 1/2 tbsp [3.7e-2 l, 37 ml, 1/4 cup, 7 1/2 tsp, 1 1/4 fl.oz]
    Foo Flour, 167 g [6 oz]
    Roasted Bar, 1/2 l [500 ml, 2 cup, 101 1/2 tsp, 33 3/4 tbsp, 17 fl.oz]
    $

That's about it.

The code can be built with stack, that is, "stack build" should be
sufficient to build it.
