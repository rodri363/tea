%This defines the key/value documentation pairs
%Then, key_print calls them, in alphabetical order.
m4_divert(-1)
m4_changequote(`<<<',`>>>') # m4 eats all quote-endquote markers, so make sure
                        # they will never appear in your text by using odd ones.
                        # Notice how these aren't the plain <> signs; 
                        # vim users, try :help digraph. 

m4_changecom(‹m4 comment:›) #Octothorpes appear in plain text.
m4_define(TeaKEY, 
<<<m4_divert(0)        
\long\def\printkey<<<>>>m4_translit($1, <<< />>>){
\begin{key}{$1}
$2
\end{key}

}
m4_divert(-1)>>>
)
