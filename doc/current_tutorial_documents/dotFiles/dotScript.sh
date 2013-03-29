cat dotInput >> connected_dot_file
cat dotFields >> connected_dot_file
cat dotChecks >> connected_dot_file
cat dotRecodes >> connected_dot_file
cat dotImpute >> connected_dot_file
dot -Tpng < connected_dot_file > connected_dot.png
