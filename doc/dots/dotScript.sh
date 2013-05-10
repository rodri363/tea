for f in dotChecks dotRecodes dotFields dotImpute dotInput; do
    sed -e  '1idigraph {node [shape=Mrecord];' -e '$a}' < ${f}_scr  |\
    dot -Tpng > ../$f.png
done

echo 'digraph { ' > connected_dot_file
echo 'node [shape=Mrecord];' >> connected_dot_file
cat dotInput_scr >> connected_dot_file
cat dotFields_scr >> connected_dot_file
cat dotChecks_scr >> connected_dot_file
cat dotRecodes_scr >> connected_dot_file
cat dotImpute_scr >> connected_dot_file
echo '}' >> connected_dot_file
dot -Tpng < connected_dot_file > connected_dot.png

