library(titanic)
library(class)
library(caret)
library(plotly)

ov_class <- function(){
    df <- as.data.frame(table(Pclass = titanic$Pclass, Survived = titanic$Survived))
    df$Survived <- factor(df$Survived, labels = c("No", "Yes"))
    df$Pclass <- factor(df$Pclass, labels = c("1st","2nd","3rd"))
    plot_ly(df, x=~Pclass, y=~Freq, color=~Survived,
    colors = c(COL_AMBER, COL_NAVY),
    type="bar", barmode="group",
    marker=list(line=list(width=0))) |> ptly()
}

ov_sex <- function (){
    df <- as.data.frame(table(Sex = titanic$Sex, Survived = titanic$Survived))
    df$Survived <- factor(df$Survived, labels = c("No", "Yes"))
    df$Sex <- factor(df$Sex, labels=c("Male","Female"))
    plot_ly(df, x=~Sex, y=~Freq, color=~Survived,
    colors = c(COL_AMBER, COL_NAVY),
    type="bar", barmode="stack",
    marker=list(line=list(width=0))) |> ptly()
}

Age <- titanic$Age[titanic$Survived == 0]

ov_age <- function(){
    plot_ly(alpha=0.6) |>
    add_histogram(x = ~Age,  name="No",
                marker=list(color=COL_AMBER)) |>
    add_histogram(x = ~titanic$Age[titanic$Survived == 1], name="Yes",
                marker=list(color=COL_NAVY)) |>
    layout(barmode="overlay") |> ptly()
}

an_pie <- function(){
    df <- as.data.frame(table(Class=titanic$Pclass))
    df$Class <- paste(c("1st","2nd","3rd"))
    plot_ly(df, labels=~Class, values=~Freq, type="pie",
            marker=list(colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
                        line=list(color=COL_CREAM, width=2)),
            textfont=list(color="#fff"), sort = FALSE) |>
      ptly() |> layout(showlegend=TRUE)
}

an_fare <- function(){
    plot_ly(titanic, y=~Fare, x=~factor(Pclass, labels=c("1st","2nd","3rd")),
            type="box", split=~factor(Pclass),
            colors=c(COL_NAVY, COL_AMBER, COL_GOLD),
            box=list(visible=TRUE), meanline=list(visible=TRUE)) |>
    ptly() |> layout(showlegend=FALSE,
    xaxis=list(title="PClass"), yaxis=list(title="Fare (£)"))
}

an_heatmap <- function(){
    df <- aggregate(as.numeric(Survived=="Yes") ~ Pclass + Sex, data=titanic, FUN=mean)
    colnames(df) <- c("Class","Sex","Rate")
    df$Label <- paste0(round(df$Rate*100), "%")
    df$Sex   <- factor(df$Sex, levels=c("male","female"), labels=c("Male","Female"))
    df$Class <- factor(df$Class, labels=c("1st","2nd","3rd"))
    plot_ly(df, x=~Sex, y=~Class, z=~Rate, type="heatmap",
            colorscale=list(c(0,COL_CREAM), c(0.5,COL_AMBER), c(1,COL_NAVY)),
            text=~Label, texttemplate="%{text}", showscale=FALSE) |>
      ptly() |> layout(xaxis=list(title=""), yaxis=list(title=""))
}

an_scatter <- function(){
    plot_ly(titanic, x=~Age, y=~Fare, color=~Survived,
            colors=c(COL_AMBER, COL_NAVY),
            type="scatter", mode="markers",
            marker=list(size=6, opacity=0.65,
                        line=list(width=0.5, color="rgba(255,255,255,0.4)")),
            text=~paste("Class:", Pclass, "<br>Sex:", Sex)) |>
      ptly() |> layout(xaxis=list(title="Age"), yaxis=list(title="Fare (£)"))
}

an_box <- function(){
    plot_ly(titanic, x=~Survived, y=~Fare, color=~Survived,
            colors=c(COL_AMBER, COL_NAVY), type="box") |>
    ptly() |> layout(showlegend=FALSE,
    xaxis=list(title="Survived"), yaxis=list(title="Fare (£)"))
}