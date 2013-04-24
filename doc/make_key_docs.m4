%This defines the key/value documentation pairs
%Then, key_print calls them, in alphabetical order.
m4_divert(-1)
m4_changequote(`<<<',`>>>') # m4 eats all quote-endquote markers, so make sure
                        # they will never appear in your text by using odd ones.

m4_changecom(‹m4 comment:›) #Octothorpes appear in plain text.
m4_define(TeaKEY, <<<
   m4_define(PrintKEY<<<>>>m4_translit($1, <<< />>>), <<<
    \begin{key}{$1}
$2
    \end{key}
    >>>)
>>>
)
