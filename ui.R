
# User Interface ----------------------------------------------------------

dashboardPagePlus(
    skin = "blue",
    enable_preloader = TRUE, loading_duration = 5, sidebar_fullCollapse = TRUE,
    title = "COVID-19 Dashboard", 
    header = dashboardHeaderPlus(
        title = span( 
                HTML("<b> COVID-19 </b>")
        ),
        userOutput("user")
    ), 
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "menu",
            menuItem(
                "Brasil",
                tabName = "br_page",
                icon = icon("globe-americas")
            ),
            menuItem(
                "Estados - RJ e SP",
                tabName = "rj_sp_page",
                icon = icon("flag")
            ),
            menuItem(
                "Mundo",
                tabName = "mundo_page",
                icon = icon("globe")
            ),
            menuItem(
                "Overview",
                tabName = "ov_page",
                icon = icon("eye")
            )
        )
    ), 
    body = dashboardBody(
        # # cor dos hiperlinks
        # tags$head(tags$style(HTML("a {color: black}"))),
        
        # Abas
        tabItems(
            brasil,
            rj_sp,
            mundo,
            overview
        )
    ), 
    rightsidebar = rightSidebar()
)
