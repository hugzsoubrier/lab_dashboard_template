# This is the HOME ui

home <- tabPanel( h3("Home"), 
                  
                  div( class = "reactive-width", 
                       
                       htmltools::includeMarkdown("tabs/home_text.Rmd"),
                       
                       div(img(src = 'png/scenarios_table.png', align = "center", height = '550px', width = '1000px'), style = "text-align: center;")
                       
                       
                  )
                  
)