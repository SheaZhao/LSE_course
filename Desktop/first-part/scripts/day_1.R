# version 3.4 or higher

install.packages("tidyverse")

# "ALT" + "-" creates "<-" automatically


## plotting
    # download data
    download.file("copy/pastelink", "folder path w/downloaded dataname.csv")
    
    # read in data
    gapminder <- read.csv("data/gapminder-FiveYearData.csv")
    gapminder
    
    library(ggplot2)
    
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
        geom_point()

    # plot relationship between life exp overtime
    ggplot(gapminder, aes(x = year, y = lifeExp, color = continent)) +
        geom_point()
    
    ggplot(gapminder, aes(x = year, y = lifeExp, by = country, color = continent)) +
        geom_line()
    
    ggplot(gapminder, aes(x = year, y = lifeExp, by = country)) +
        geom_line(aes(color = continent)) +
        geom_point()
    
    # order or geom_point & geom_line switches which is in front
    ggplot(gapminder, aes(x = year, y = lifeExp, by = country)) +
        geom_line(aes(color = continent)) +
        geom_point(color = "tomato")
    
    
    ggplot(gapminder, aes(x = year, y = lifeExp, by = country)) +
        geom_point(color = "tomato") +
        geom_line(aes(color = continent))
        
    
## transformation & stats
    
    # to stretch our, use scale_x_log10() 
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        scale_x_log10()
    
    # add transparency so you can see overlap concentration w/ alpha
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point(alpha = .5) +
        scale_x_log10()
    
    # make earlier years more transparent
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point(aes(alpha = year)) +
        scale_x_log10()
    
    # could reverse it using rev function - the legend is wrong, bug?
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point(aes(alpha = rev(year))) +
        scale_x_log10()
    
    
    # linear trend?
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point(alpha = .5) +
        scale_x_log10() +
        geom_smooth(method = lm)
    
    # just one line instead of one for each continent
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp )) +
        geom_point(aes(color = continent), alpha = .5) +
        scale_x_log10() +
        geom_smooth(method = "lm")
    
    # change color of the points - find colors w/ colors function
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp )) +
        geom_point(aes(color = "blue2"), alpha = .5) +
        scale_x_log10() +
        geom_smooth(method = "lm", aes(color = "violet"))
    
    
    # change shape of the points
    ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
        geom_point(aes(color = "blue2", shape = continent), alpha = .5) +
        scale_x_log10() +
        geom_smooth(method = "lm", aes(color = "violet"))
    
    
## Data frames
    
    dim(gapminder) #objs & variables
    nrow()
    ncol()
    str() # tells obj type, obs., rowns, col obj type, etc.
    
    
    gapminder$country # extract column in form of a vector
    
    le <- gapminder$lifeExp
    str(le)
    country <- gapminder$country
    class(country) # gives class of the obj (vector in this case)
    
    # think of vectors as egg cartons - can only have one type of egg??
    
    
    
## cat data
    cats <- read.csv("data/feline-data.csv")
    
    cats
    str(cats)
    class(cats$likes_string)
    
    paste("My cat is", cats$coat) # interesting
    
    paste("My cat is", cats$weight) 
    
    cats$coat + cats$weight # error b/c can't combine characters & numbers
    
    str(cats)
    
    
    cat_name <- c("booboo", "pongo", "holly")
    
    cats_2 <- data.frame(
        name = cat_name,
        coat = cats$coat,
        weight = cats$weight,
        likes_string = cats$likes_string
    )
    
    str(cats_2)
    
    # to keep names as characters not factors add stringsAsFactors = FALSE
    
    cats_2 <- data.frame(
        name = cat_name,
        coat = cats$coat,
        weight = cats$weight,
        likes_string = cats$likes_string,
        stringsAsFactors = FALSE
    )
    
    str(cats_2) # now names are chr
    
    
    # also w/ read.csv("data/feline-data.csv", stringsAsFactors = FALSE)
    # so coats doesn't convert to factors
    
    # so chr represents objs as numbers in R and factors are categorical (like alphabetical?)
    
    
    # force vector to int
    
        combine_vector <- c(2, 6, 3)
        str(combine_vector) # num
        combine_vector <- c(2L, 6L, 3L)
        str(combine_vector) # int
        
        
        combine_vector <- integer(3) # 0 0 0
        combine_vector [1] <- 2 #2 0 0 
        combine_vector <- c(2, "cat", 3) # chr
    
    # logical values
        quiz_vector <- c(TRUE, FALSE, TRUE) # class = logical
        sum(quiz_vector) # get 3 b/c T = 1 & F = 0
        
        cats$weight > 3 #FALSE  TRUE  TRUE
        
        mean(cats$weight > 3) #0.6666667
        
        
        
## subsetting
    cat_name[1]
    cat_name[c(1,3)]
    cat_name[grepl("^p", cat_name)]  # only cat names that start w/ p  
    
    cat_name[-2] # remove 2nd element
    cat_name[-c(1,3)] # remove more than one
    
    
    cats[1,] # 1st row
    cats[,2] #2nd col
    cats[3,1] # 3rd element of the 2nd col
    
    
## subset dataframe to only include cats who like strings
    like_string <- cats_2$name[cats_2$likes_string == 1]
    like_string
    # the answer you will get is: "booboo"
    
## name of the cats that weigh less than 3 pounds
    cats_less3 <- cats_2$name[cats_2$weight < 3]
    cats_less3
    # the answer you will get is: "booboo" "holly" 
   
    
    cats_3 <- cat_name[cat_name$weight < 3, "name"] 
    
    
# tables    
    tbl_country <- table(gapminder$country) [1:6]
    tbl_country
    
    str(tbl_country)
    
    tbl_country[names(tbl_country) == "Australia"]
    
    tbl_country[names(tbl_country) == "Australia" | names(tbl_country) == "Argentina"] 
                 
    tbl_country[names(tbl_country) %in% c("Australia", "Argentina", "Albania")]
    
    
    head(gapminder)
    sub_gapminder <- gapminder[gapminder$country %in% c("China", "Chad", "France", "India", "England", "Chile"),]
    sub_gapminder
    
    
    
    