library(titanic)
library(class)
library(caret)
library(plotly)
library(dplyr)

titanic <- titanic::titanic_train

# ensure correct types
titanic$Survived <- as.numeric(titanic$Survived)
titanic$Pclass   <- as.numeric(titanic$Pclass)
titanic <- na.omit(titanic)


ptly <- function(p) {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)"
  )
}

COL_NAVY  <- "#344C65"
COL_AMBER <- "#AC6C35"
COL_GOLD  <- "#b89018"
COL_CREAM <- "#DBCEBF"
"#976132"

kpi <- function(value, label, icon_name, css_class) {
  div(class = paste("kpi-tile", css_class),
      div(style = "padding-top: 10px;",
          div(class = "kpi-icon", icon(icon_name)),
          div(class = "kpi-val",  value),
          div(class = "kpi-lbl",  label)
      )
  )
}

ov_class <- function(){
  df <- as.data.frame(table(Pclass = titanic$Pclass, Survived = titanic$Survived))
  df$Survived <- factor(df$Survived, levels = c(0,1), labels = c("No", "Yes"))
  df$Pclass <- factor(df$Pclass, labels = c("1st","2nd","3rd"))
  
  plot_ly(df, x=~Pclass, y=~Freq, color=~Survived,
          colors = c(COL_AMBER, COL_NAVY),
          type="bar", barmode="group",
          marker=list(line=list(width=0))) |> ptly()
}

ov_sex <- function (){
  df <- as.data.frame(table(Sex = titanic$Sex, Survived = titanic$Survived))
  df$Survived <- factor(df$Survived, levels = c(0,1), labels = c("No", "Yes"))
  df$Sex <- factor(df$Sex, levels = c("male", "female"), labels=c("Male","Female"))
  
  plot_ly(df, x=~Sex, y=~Freq, color=~Survived,
          colors = c(COL_AMBER, COL_NAVY),
          type="bar", barmode="stack",
          marker=list(line=list(width=0))) |> ptly()
}

# precompute Age groups
Age  <- titanic$Age[titanic$Survived == 0]
Age_Yes <- titanic$Age[titanic$Survived == 1]

ov_age <- function(){
  plot_ly(alpha=0.6) |>
    add_histogram(x = ~Age,  name="No",
                  marker=list(color=COL_AMBER)) |>
    add_histogram(x = ~Age_Yes, name="Yes",
                  marker=list(color=COL_NAVY)) |>
    layout(barmode="overlay") |> ptly()
}

an_pie <- function(){
  df <- as.data.frame(table(Class=titanic$Pclass))
  df$Class <- c("1st","2nd","3rd")
  
  plot_ly(df, labels=~Class, values=~Freq, type="pie",
          marker=list(colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
                      line=list(color=COL_CREAM, width=2)),
          textfont=list(color="#fff"), sort = FALSE) |>
    ptly() |> layout(showlegend=TRUE)
}

an_fare <- function(){
  plot_ly(titanic,
          y=~Fare,
          x=~factor(Pclass, labels=c("1st","2nd","3rd")),
          type="violin", box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          split=~factor(Pclass),
          colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
          box=list(visible=TRUE),
          meanline=list(visible=TRUE)) |>
    ptly() |> layout(
      showlegend=FALSE,
      xaxis=list(title="PClass"),
      yaxis=list(title="Fare (£)")
    )
}

an_heatmap <- function(){
  df <- aggregate(Survived ~ Pclass + Sex, data=titanic, FUN=mean)
  
  colnames(df) <- c("Class","Sex","Rate")
  df$Label <- paste0(round(df$Rate*100), "%")
  
  df$Sex   <- factor(df$Sex, levels=c("male","female"), labels=c("Male","Female"))
  df$Class <- factor(df$Class, labels=c("1st","2nd","3rd"))
  
  plot_ly(df,
          x=~Sex, y=~Class, z=~Rate,
          type="heatmap",
          colorscale=list(
            c(0, COL_CREAM),
            c(0.5, COL_AMBER),
            c(1, COL_NAVY)
          ),
          text=~Label,
          texttemplate="%{text}",
          showscale=FALSE) |>
    ptly() |> layout(
      xaxis=list(title="Sex"),
      yaxis=list(title="PClass")
    )
}

an_scatter <- function(){
  trend <- lm(Fare ~ Age, data = titanic)
  trend_df <- data.frame(Age = sort(titanic$Age))
  trend_df$Fare <- predict(trend, trend_df)

  plot_ly(titanic,
          x=~Age, y=~Fare,
          color=~factor(Survived, levels=c(0,1), labels=c("No","Yes")),
          colors=c(COL_AMBER, COL_NAVY),
          type="scatter", mode="markers",
          marker=list(
            size=6,
            opacity=0.65,
            line=list(width=0.5, color="rgba(255,255,255,0.4)")
          ),
          text=~paste("Class:", Pclass, "<br>Sex:", Sex)) |>
    add_lines(data=trend_df, x=~Age, y=~Fare,
      line=list(color=COL_GOLD, width=2),
      name="Trend", inherit=FALSE, showlegend=TRUE) |>
    ptly() |> layout(
      xaxis=list(title="Age"),
      yaxis=list(title="Fare (£)")
    )
}

an_box <- function(){
  plot_ly(titanic,
          x=~factor(Survived, levels=c(0,1), labels=c("No","Yes")),
          y=~Fare,
          color=~factor(Survived, levels=c(0,1), labels=c("No","Yes")),
          colors=c(COL_AMBER, COL_NAVY),
          type="violin", box = list(visible = TRUE),
          meanline = list(visible = TRUE)) |>
    ptly() |> layout(
      showlegend=FALSE,
      xaxis=list(title="Survived"),
      yaxis=list(title="Fare (£)")
    )
}

# anakin parts

an_age_hist <- function(){
  plot_ly(alpha = 0.75) |>
    add_histogram(x = ~Age_No,  name = "No",
                  xbins = list(start = 0, end = 80, size = 5),
                  marker = list(color = COL_AMBER)) |>
    add_histogram(x = ~Age_Yes, name = "Yes",
                  xbins = list(start = 0, end = 80, size = 5),
                  marker = list(color = COL_NAVY)) |>
    layout(barmode = "stack",
           xaxis = list(title = "Age"),
           yaxis = list(title = "Count")) |>
    ptly()
}

an_embark <- function(){
  df <- titanic |>
    filter(Embarked != "") |>
    mutate(Embarked = recode(Embarked,
                             C = "Cherbourg", Q = "Queenstown", S = "Southampton")) |>
    count(Embarked, Survived) |>
    mutate(Survived = factor(Survived, levels = c(0,1), labels = c("No","Yes")))
  
  plot_ly(df, x = ~Embarked, y = ~n, color = ~Survived,
          colors = c(COL_AMBER, COL_NAVY),
          type = "bar", barmode = "stack",
          marker = list(line = list(width = 0))) |>
    ptly() |> layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Passengers")
    )
}

an_family <- function(){
  df <- titanic |>
    dplyr::mutate(FamilySize = SibSp + Parch) |>
    dplyr::group_by(FamilySize) |>
    dplyr::summarise(SurvivalRate = mean(Survived) * 100, n = n(), .groups = "drop") |>
    dplyr::filter(n >= 10)
  
  plot_ly(df, x = ~factor(FamilySize), y = ~SurvivalRate,
          type = "bar",
          marker = list(
            color = ifelse(df$SurvivalRate >= 50, COL_NAVY, COL_AMBER),
            line  = list(width = 0)
          )) |>
    ptly() |> layout(
      xaxis = list(title = "Family size (SibSp + Parch)"),
      yaxis = list(title = "Survival rate (%)", range = c(0, 100))
    )
}
