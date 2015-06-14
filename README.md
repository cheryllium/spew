# spew
easy web scaffolding with lisp | scaffold parenthetically for easy webpages

## what it does 

Spew creates simple layouts for you so you don't have to mess around with HTML and CSS. Instead, you can specify 
what you'd like your webpage to look like using nested rows and columns of divs, and Spew will generate the HTML 
and CSS files for you. 

Please note that Spew does not have advanced stylesheet capabilities at the moment, and should only be used to generate 
webpage skeletons, which you can then fill out with your content and extra styles. 

## how it works

### plists represent divs 

    (:content "This is a div.")
     
Each div in your website is represented by a property list. The property `content` is not optional, and represents the 
inner HTML of the div. 

    (:content "This is a div." 
     :styles "background-color: #f00;")
     
The property `styles` is optional. Here you can insert a few lines of CSS specific to this div. 

### rows and cols organize divs

The `ROWS` and `COLS` functions do all of the work in Spew. These each take as an argument a single list of plists 
(in other words, a list of divs) and return a list of div plists as well. The `ROWS` function will organize its 
divs in rows (each div will be a separate row) while the `COLS` function organizes them into columns. 

    (rows 
      '((:content "Div number one.")
        (:content "Div number two.")))

The above code will generate a webpage with two rows. The first row will say "Div number one", and the second will 
say "Div number two". 

### you can nest rows and cols

Of course, this utility would not be very useful if you could not nest rows and columns. Since `ROWS` and `COLS` 
evaluate to the same kind of data as their arguments, they can be passed into each other. For example, consider: 

    (cols 
      (list 
        (rows 
          '((:content "Test div please ignore")
            (:content "Second test div")))
        (rows 
          '((:content "Another div")
            (:content "One more div")
            (:content "Final div")))))
          
This example organizes the web page into two columns, each containing several rows of divs. 

### make-files 

To generate the HTML and CSS, pass the call to `ROWS` or `COLS` to the function `MAKE-FILES`. For an example, please see the 
function `EXAMPLE` at the bottom of `spew.lisp`. 

## Notes

* The `:styles` property currently takes lines of actual CSS code. This is not too cool and I need to make it so that 
it can take CSS in some sort of s-expression format. That would be way cooler. 

* Some of the HTML is hard-coded. It would be cool to incorporate LHTML instead of having actual HTML strings in my code. 

* By default, each div will take up an equal amount of space. I just decided this would be best. 
You can easily change the width/height percentages in the generated CSS file. 
