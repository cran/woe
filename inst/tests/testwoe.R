
test1<-woe(mtcars,"mpg",TRUE,"am",10)

test2<-woe(mtcars,"cyl",FALSE,"am",10)

test3<-woe(mtcars,"mpg_wrong",TRUE,"am",10)

test4<-woe(mtcars,"cyl_wrong",FALSE,"am",10)

expect_equal(class(test1),"data.frame")
expect_equal(class(test2),"data.frame")


expect_equal(class(test3),"numeric")
expect_equal(class(test4),"numeric")


