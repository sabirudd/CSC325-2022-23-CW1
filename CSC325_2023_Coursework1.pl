%%% Rules %%%

s(s(NP)) --> np_singular(NP); np_plural(NP).
s(s(NP, VP)) --> n_singular_animate(NP), vp_singular(VP); n_plural_animate(NP), vp_plural(VP); 
                 n_singular_inanimate(NP), vp_plural(VP); n_plural_inanimate(NP), vp_singular(VP);
                 n_singular_inanimate(NP), vp_singular(VP); n_plural_inanimate(NP), vp_plural(VP).
s(s(P, VP)) --> np_singular_subject(P), vp_plural(VP); np_plural_subject(P), vp_singular(VP).

% Adjectives
adj(adj(Word)) --> [Word], {lex(Word, adj)}.
adj(adj(Word, A)) --> [Word], {lex(Word, adj)}; [Word], {lex(Word, adj)}, adj(A).


% Singular - Det, Nouns + Verbs
det_singular(det(Word)) --> [Word], {lex(Word, det, singular)}.
np_singular(np(Det, N)) --> det_singular(Det), n_singular(N).
np_singular(np(Det, N, PP)) --> det_singular(Det), n_singular(N), pp(PP).
np_singular(np(Det, A, N)) --> det_singular(Det), adj(A), n_singular(N).
np_singular(np(Det, A, N, PP)) --> det_singular(Det), adj(A), n_singular(N), pp(PP).
n_singular(n(Word)) --> [Word], {lex(Word, n, singular, _)}; [Word], {lex(Word, n, singular, inanimate)}.

n_singular_animate(np(Det, N)) --> det_singular(Det), n_singular_animate(N).
n_singular_animate(np(Det, N, PP)) --> det_singular(Det), n_singular_animate(N), pp(PP).
n_singular_animate(np(Det, A, N)) --> det_singular(Det), adj(A), n_singular_animate(N).
n_singular_animate(np(Det, A, N, PP)) --> det_singular(Det), adj(A), n_singular_animate(N), pp(PP).
n_singular_animate(n(Word)) --> [Word], {lex(Word, n, singular, animate)}.

n_singular_inanimate(np(Det, N)) --> det_singular(Det), n_singular_inanimate(N).
n_singular_inanimate(np(Det, N, PP)) --> det_singular(Det), n_singular_inanimate(N), pp(PP).
n_singular_inanimate(np(Det, A, N)) --> det_singular(Det), adj(A), n_singular_inanimate(N).
n_singular_inanimate(np(Det, A, N, PP)) --> det_singular(Det), adj(A), n_singular_inanimate(N), pp(PP).
n_singular_inanimate(n(Word)) --> [Word], {lex(Word, n, singular, inanimate)}.

vp_singular(vp(V, NP)) --> v_singular_transitive_3(V), np_plural(NP);
                           v_singular_transitive_3(V), np_singular(NP);
                           v_singular_transitive_2(V), np_singular(NP);
                           v_singular_transitive_1(V), np_singular(NP).
vp_singular(vp(V)) --> v_singular_intransitive(V).
v_singular_transitive_1(v_tv(Word)) --> [Word], {lex(Word, tv, singular, 1)}.
v_singular_transitive_2(v_tv(Word)) --> [Word], {lex(Word, tv, singular, 2)}.
v_singular_transitive_3(v_tv(Word)) --> [Word], {lex(Word, tv, singular, 3)}.
v_singular_intransitive(v_iv(Word)) --> [Word], {lex(Word, iv, singular, _)}.


% Plural - Det, Nouns + Verbs
det_plural(det(Word)) --> [Word], {lex(Word, det, plural)}.
np_plural(np(Det, N)) --> det_plural(Det), n_plural(N).
np_plural(np(Det, N, PP)) --> det_plural(Det), n_plural(N), pp(PP).
np_plural(np(Det, A, N)) --> det_plural(Det), adj(A), n_plural(N).
np_plural(np(Det, A, N, PP)) --> det_plural(Det), adj(A), n_plural(N), pp(PP).
n_plural(n(Word)) --> [Word], {lex(Word, n, plural, _)}.

n_plural_animate(np(Det, N)) --> det_plural(Det), n_plural_animate(N).
n_plural_animate(np(Det, N, PP)) --> det_plural(Det), n_plural_animate(N), pp(PP).
n_plural_animate(np(Det, A, N)) --> det_plural(Det), adj(A), n_plural_animate(N).
n_plural_animate(np(Det, A, N, PP)) --> det_plural(Det), adj(A), n_plural_animate(N), pp(PP).
n_plural_animate(n(Word)) --> [Word], {lex(Word, n, plural, animate)}.

n_plural_inanimate(np(Det, N)) --> det_plural(Det), n_plural_inanimate(N).
n_plural_inanimate(np(Det, N, PP)) --> det_plural(Det), n_plural_inanimate(N), pp(PP).
n_plural_inanimate(np(Det, A, N)) --> det_plural(Det), adj(A), n_plural_inanimate(N).
n_plural_inanimate(np(Det, A, N, PP)) --> det_plural(Det), adj(A), n_plural_inanimate(N), pp(PP).
n_plural_inanimate(n(Word)) --> [Word], {lex(Word, n, plural, inanimate)}.

vp_plural(vp(V, NP)) --> v_plural_transitive_3(V), np_singular(NP);
                         v_plural_transitive_3(V), np_plural(NP);
                         v_plural_transitive_2(V), np_plural(NP);
                         v_plural_transitive_1(V), np_plural(NP).
vp_plural(vp(V)) --> v_plural_intransitive(V).
v_plural_transitive_1(v_tv(Word)) --> [Word], {lex(Word, tv, plural, 1)}.
v_plural_transitive_2(v_tv(Word)) --> [Word], {lex(Word, tv, plural, 2)}.
v_plural_transitive_3(v_tv(Word)) --> [Word], {lex(Word, tv, plural, 3)}.
v_plural_intransitive(v_iv(Word)) --> [Word], {lex(Word, iv, plural, _)}.

np_singular_subject(np(P)) --> pronoun_singular_subject(P).
pronoun_singular_subject(p(Word)) --> [Word], {lex(Word, pronoun, singular, 1, subject)}; [Word], {lex(Word, pronoun, singular, 2, subject)}.
np_plural_subject(np(P)) --> pronoun_plural_subject(P).
pronoun_plural_subject(p(Word)) --> [Word], {lex(Word, pronoun, singular, 3, subject)}; [Word], {lex(Word, pronoun, plural, _, subject)}.

pronoun_object(p(Word)) --> [Word], {lex(Word, pronoun, _, _, object)}.
np_singular(np(P)) --> pronoun_singular_object(P).
pronoun_singular_object(p(Word)) --> [Word], {lex(Word, pronoun, singular, 1, object)}; [Word], {lex(Word, pronoun, singular, 2, object)}.
np_plural(np(P)) --> pronoun_plural_object(P).
pronoun_plural_object(p(Word)) --> [Word], {lex(Word, pronoun, singular, 3, object)}; [Word], {lex(Word, pronoun, plural, _, object)}.


% Prepositions
prep(prep(Word)) --> [Word], {lex(Word, prep)}.
pp(pp(PP, NP)) --> prep(PP), np_singular(NP); prep(PP), np_plural(NP).





%%% LEXICON %%%

%% Determiners Lexicon (word, type, number) %%
lex(the, det, _).
lex(a, det, singular).
lex(two, det, plural).


%% Pronouns Lexicon (word, type, number, person, role) %%
lex(i, pronoun, singular, 1, subject).
lex(you, pronoun, singular, 2, subject).
lex(he, pronoun, singular, 3, subject).
lex(she, pronoun, singular, 3, subject).
lex(it, pronoun, singular, 3, subject).
lex(we, pronoun, plural, 1, subject).
lex(you, pronoun, plural, 2, subject).
lex(they, pronoun, plural, 3, subject).

lex(me, pronoun, singular, 1, object).
lex(you, pronoun, singular, 2, object).
lex(him, pronoun, singular, 3, object).
lex(her, pronoun, singular, 3, object).
lex(it, pronoun, singular, 3, object).
lex(us, pronoun, plural, 1, object).
lex(you, pronoun, plural, 2, object).
lex(them, pronoun, plural, 3, object).


%Verbs Lexicon (word, type, number, person)  --  tv = transitive, iv = intransitive
lex(know, tv, singular, 1).
lex(know, tv, singular, 2).
lex(knows, tv, singular, 3).
lex(know, tv, plural, _).
lex(see, tv, singular, 1).
lex(see, tv, singular, 2).
lex(sees, tv, singular, 3).
lex(see, tv, plural, _).
lex(hire, tv, singular, 1).
lex(hire, tv, singular, 2).
lex(hires, tv, singular, 3).
lex(hire, tv, plural, _).

lex(fall, iv, singular, 1).
lex(fall, iv, singular, 2).
lex(falls, iv, singular, 3).
lex(fall, iv, plural, _).
lex(sleep, iv, singular, 1).
lex(sleep, iv, singular, 2).
lex(sleeps, iv, singular, 3).
lex(sleep, iv, plural, _).


%Nouns Lexicon (word, type, number, animacy)
lex(man, n, singular, animate).
lex(woman, n, singular, animate).
lex(apple, n, singular, inanimate).
lex(chair, n, singular, inanimate).
lex(room, n, singular, inanimate).

lex(men, n, plural, animate).
lex(women, n, plural, animate).
lex(apples, n, plural, inanimate).
lex(chairs, n, plural, inanimate).
lex(rooms, n, plural, inanimate).


%Prepositions (word, type)
lex(on, prep).
lex(in, prep).
lex(under, prep).


%Adjective Lexicon (word, type)
lex(old, adj).
lex(young, adj).
lex(red, adj).
lex(short, adj).
lex(tall, adj).





/* Input Queries 

    1. the woman sees the apples
        Input   -   s(Tree,[the,woman,sees,the,apples],[]).
        Output  -   Tree = s(np(det(the), n(woman)), vp(v_tv(sees), np(det(the), n(apples)))) .
    2. a woman knows him
        Input   -   s(Tree,[a,woman,knows,him],[]).
        Output  -   Tree = s(np(det(a), n(woman)), vp(v_tv(knows), np(p(him)))) .
    3. *two woman hires a man
        Input   -   s(Tree,[two,woman,hires,a,man],[]).
        Output  -   false.
    4. two women hire a man
        Input   -   s(Tree,[two,women,hire,a,man],[]).
        Output  -   Tree = s(np(det(two), n(women)), vp(v_tv(hire), np(det(a), n(man)))) .
    5. she knows her
        Input   -   s(Tree,[she,knows,her],[]).
        Output  -   Tree = s(np(p(she)), vp(v_tv(knows), np(p(her)))) .
    6. *she know the man  ->  incorrect
        Input   -   s(Tree,[she,knows,the,man],[]).
        Output  -   Tree = s(np(p(she)), vp(v_tv(knows), np(det(the), n(man)))) .
    7. *us see the apple
        Input   -   s(Tree,[us,see,the,apple],[]).
        Output  -   false.
    8. we see the apple
        Input   -   s(Tree,[we,see,the,apple],[]).
        Output  -   Tree = s(np(p(we)), vp(v_tv(see), np(det(the), n(apple)))) 
    9. i know a short man
        Input   -   s(Tree,[i,know,a,short,man],[]).
        Output  -   Tree = s(np(p(i)), vp(v_tv(know), np(det(a), adj(short), n(man)))) .
    10. *he hires they
        Input   -   s(Tree,[he,hires,they],[]).
        Output  -   false.
    11. two apples fall
        Input   -   s(Tree,[two,apples,fall],[]).
        Output  -   Tree = s(np(det(two), n(apples)), vp(v_iv(fall))) .
    12. the apple falls
        Input   -   s(Tree,[the,apple,falls],[]).
        Output  -   Tree = s(np(det(the), n(apple)), vp(v_iv(falls))) .
    13. the apples fall
        Input   -   s(Tree,[the,apples,fall],[]).
        Output  -   Tree = s(np(det(the), n(apples)), vp(v_iv(fall))) .
    14. i sleep
        Input   -   s(Tree,[i,sleep],[]).
        Output  -   Tree = s(np(p(i)), vp(v_iv(sleep))) .
    15. you sleep
        Input   -   s(Tree,[you,sleep],[]).
        Output  -   Tree = s(np(p(you)), vp(v_iv(sleep))) .
    16. she sleeps
        Input   -   s(Tree,[she,sleeps],[]).
        Output  -   Tree = s(np(p(she)), vp(v_iv(sleeps))) .
    17. *he sleep  ->  incorrect
        Input   -   s(Tree,[he,sleep],[]).
        Output  -   Tree = s(np(p(he)), vp(v_iv(sleep))) 
    18. *them sleep
        Input   -   s(Tree,[them,sleep],[]).
        Output  -   false.
    19. *a men sleep
        Input   -   s(Tree,[a,men,sleep],[]).
        Output  -   false.
    20. *the tall woman sees the red
        Input   -   s(Tree,[the,tall,woman,sees,the,red],[]).
        Output  -   false.
    21. the young tall man knows the old short woman
        Input   -   s(Tree,[the,young,tall,man,knows,the,old,short,woman],[]).
        Output  -   Tree = s(np(det(the), adj(young, adj(tall)), n(man)), vp(v_tv(knows), np(det(the), adj(old, adj(short)), n(woman)))) .
    22. *a man tall knows the short woman
        Input   -   s(Tree,[a,man,tall,know,the,short,woman],[]).
        Output  -   false.
    23. a man on a chair sees a woman in a room
        Input   -   s(Tree,[a,man,on,a,chair,sees,a,woman,in,a,room],[]).
        Output  -   Tree = s(np(det(a), n(man), pp(prep(on), np(det(a), n(chair)))), vp(v_tv(sees), np(det(a), n(woman), pp(prep(in), np(det(a), n(room)))))) .
    24. *a man on a chair sees a woman a room in
        Input   -   s(Tree,[a,man,on,a,chair,sees,a,womam,a,room,in],[]).
        Output  -   false.
    25. the tall young woman in a room on the chair in a room in the room sees the red apples under the chair
        Input   -   s(Tree,[the,tall,young,woman,in,a,room,on,the,chair,in,a,room,in,the,room,sees,the,red,apples,under,the,chair],[]).
        Output  -   Tree = s(np(det(the), adj(tall, adj(young)), n(woman), pp(prep(in), np(det(a), n(room), pp(prep(on), np(det(the), n(chair), pp(prep(in), np(det(a), n(room), pp(prep(...), np(..., ...))))))))), vp(v_tv(sees), np(det(the), adj(red), n(apples), pp(prep(under), np(det(the), n(chair)))))) .
    26. the woman sees the apples
        Input   -   s(Tree,[the,woman,sees,the,apples],[]).
        Output  -   Tree = s(np(det(the), n(woman)), vp(v_tv(sees), np(det(the), n(apples)))) .
    27. a woman knows him
        Input   -   s(Tree,[a,woman,knows,him],[]).
        Output  -   Tree = s(np(det(a), n(woman)), vp(v_tv(knows), np(p(him)))) .
    28. the man sleeps
        Input   -   s(Tree,[the,man,sleeps],[]).
        Output  -   Tree = s(np(det(the), n(man)), vp(v_iv(sleeps))) .
    29. *the room sleeps
        Input   -   s(Tree,[the,room,sleeps],[]).
        Output  -   false.
    30. *the apple sees the chair
        Input   -   s(Tree,[the,apple,sees,the,chair],[]).
        Output  -   false.
    31. *the rooms know the man
        Input   -   s(Tree,[the,rooms,know,the,man],[]).
        Output  -   false.
    32. the apple falls
        Input   -   s(Tree,[the,apple,falls],[]).
        Output  -   Tree = s(np(det(the), n(apple)), vp(v_iv(falls))) .
    33. the man falls
        Input   -   s(Tree,[the,man,falls],[]).
        Output  -   Tree = s(np(det(the), n(man)), vp(v_iv(falls))) .
*/
