polysemous
==========

Some old university work in Prolog on word sense disambiguation.

Shoving it here for safekeeping and reference for anyone who
wants to see how not to write good Prolog.

Here be dragons!

In Summary
----------

Polysemy causes inaccuracy in information retrieval by allowing
a word to have more than one meaning, for example: 

    Dog: someone who is morally reprehensible.
    Dog: a member of the genus Canis.

Polysemy causes the results of a search to be inaccurate by 
including all the documents containing the unwanted senses of 
a word.

    I dig that groovy cat!

This project is an attempt to increase the accuracy of such 
queries by reducing the problems associated with polysemy by 
identifying the meaning of each word in a document (a process 
called sense tagging) and using those senses in place of words 
to search for a document.

We attempt to achieve this by making use of the [Lesk algorithm](http://en.wikipedia.org/wiki/Lesk_algorithm), 
the [Vector Space Model](http://en.wikipedia.org/wiki/Vector_space_model), and the [Wordnet project](http://wordnet.princeton.edu/).

It is a bit like, but not quite the same as [Latent Semantic Indexing](http://en.wikipedia.org/wiki/Latent_semantic_indexing).
