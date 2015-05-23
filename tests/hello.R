library(tea)
readSpec("hello.spec")
doInput()

tt <- teaTable("dc", limit=20, cols=c("serialno", "agep", "schl"))
print(tt)

print(summary(teaTable("dc", cols=c("agep", "schl"))))
