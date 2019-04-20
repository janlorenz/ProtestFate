;; ====================================================== DECLARATIONS ======================================================

extensions [ Rnd ] ;; for roulette wheel selection / for network generation
directed-link-breed [followers follower]
;; A follower is a directed link which means that the source of the link reads messages from the target of the link
;; in-follower-neighbors are all who follow the turtle (they read its message)
;; out-follower-neighbors are all whom the turtle follows (it reads their messages)
globals [ used-random-seed ] ;; outputting the random seed
turtles-own [
  concerns                       ;; list of length *topics* containing the turtle's concerns on each topic, concerns range from 0 to *concern-steps*
  protest-threshold              ;; individually assigned protest threshold between 0 and 1
  protest-status                 ;; from {"concern","social","no"}
  protest-topic                ;; from {1,...,*num-topics*}, topic selected for protesting (e.g. poster / badge / slogan), or zero if not protesting
  news-feed                      ;; list of protest-topic's from all following (protesters post that they are protesting)
  fraction-protesting-following  ;; the fraction of protesting people from all following
  clique                         ;; clique id for generation of cliques in friends network
]

;; ====================================================== SETUP PROCEDURES ======================================================
to setup
  ifelse not keep-network or count turtles != population [
    clear-all
    setup-seed
    setup-network
  ]
  [
    clear-all-plots
    setup-seed
  ]
  setup-protest-mechanisms
  setup-concerns
  setup-thresholds
  colorize
  if count turtles with [size = 1] = population [ask turtles [set size sqrt (count in-follower-neighbors / mean [count in-follower-neighbors] of turtles)]] ; only resize when new network generated
  reset-ticks
end

to setup-seed
  set used-random-seed ifelse-value (randomSeed?) [ randomSeed ] [ new-seed ]
  random-seed used-random-seed
  output-print used-random-seed
end

to setup-protest-mechanisms
  if protest-mechanisms != "manual" [
    set concern-protest member? "cp" protest-mechanisms
    set social-activation member? "sa" protest-mechanisms
    set social-media-concern member? "smc" protest-mechanisms
  ]
end

to setup-network
  ; follower network (directed)
  ifelse (following > 0) [
    repeat population [
    create-turtles 1 [
      let tofollow ifelse-value (count other turtles <= following) [other turtles]
        [rnd:weighted-n-of following (other turtles) [1 + count in-follower-neighbors]]
      create-followers-to tofollow
    ]]] [
    create-turtles population
  ]
  ask turtles [ set clique random round (population / friends) ]
  ; friends network (undirected / reciprocal)
  if friends-network = "random" [ ask turtles [
    ask turtles with [ who > [ who ] of myself ] [
      if random-float 1 < friends / population [
        create-follower-to myself
        create-follower-from myself
  ]]]]
  if friends-network = "ring" and friends >= 2 [ ask turtles [
    foreach (n-values (friends / 2) [i -> i + 1]) [ n ->
      create-follower-to turtle ((who + n) mod count turtles)
      create-follower-from turtle ((who + n) mod count turtles)
  ]]]
  if friends-network = "cliques" [
    ask turtles [ set clique random round (population / friends) ]
    ask turtles [ create-followers-to other turtles with [clique = [clique] of myself] ]
  ]
  ; layout
  ask turtles [setxy random-xcor random-ycor]
  repeat 30 [ layout-spring turtles followers 0.2 5 10 ]
end

to setup-concerns
  ask turtles [
    set concerns n-values num-topics [random-binomial max-concern initial-concern-level]
    ; set concerns n-values num-topics [0] set concerns replace-item (clique mod num-topics) concerns (random-binomial max-concern initial-concern-level)
    set protest-topic 0
    set protest-status "no"
  ]
  setup-plots
end

to setup-thresholds
  ask turtles [
    set protest-threshold random-normal threshold-level threshold-dispersion
    set protest-status "no"
  ]
  setup-plots
end

;; ====================================================== GO PROCEDURES =================================================

to go
  let num-protest count turtles with [protest-status != "no"]
  ask turtles [ read-news-feed ]
  ask turtles [ decide-protest-or-concern-increase ]
  colorize
  tick
  if (count turtles with [protest-status = "concern"] = count turtles with [protest-threshold <= 1]) or
     (count turtles with [protest-status = "no"] = count turtles) or
     (concern-protest and not social-activation and not social-media-concern and ticks = 1) or
     (social-activation and not social-media-concern and num-protest = count turtles with [protest-status != "no"])
     [stop]
end

to read-news-feed ; turtles not already in concern protest collect the concerned topics of following and compute the fraction of following who protest
  if max concerns < protest-threshold * max-concern or not concern-protest [
    if social-media-concern [ set news-feed [protest-topic] of out-follower-neighbors ]
    if social-activation [ set fraction-protesting-following ifelse-value (not any? out-follower-neighbors) [0]
      [ count out-follower-neighbors with [protest-status != "no"] / count out-follower-neighbors ]
    ]
  ]
end ; Note: This procedure is executed before decide-protest-or-concern-increase for all agents to avoid order effects

to decide-protest-or-concern-increase ; turtles check if the do concern protest, otherwise concern increases on topic from newsfeed and they might protest through social activation
  set protest-status "no"
  set protest-topic 0
  ifelse (concern-protest and max concerns >= protest-threshold * max-concern) [ ; genuine protester because at least one concern is above threshold
    set protest-status "concern"
    let relevant-concerns map [i -> ifelse-value (i >= protest-threshold * max-concern) [i] [0] ] concerns
    set protest-topic random-select-topic relevant-concerns
  ][
    if social-media-concern [
      let selected-topic ifelse-value (empty? news-feed) [0] [one-of news-feed]
      if (selected-topic != 0) [ set concerns increased-concern concerns selected-topic ]
    ]
    if (social-activation and fraction-protesting-following >= protest-threshold) [ ; protester because many following are protesting
      set protest-status "social"
      set protest-topic random-select-topic concerns
    ]
   ]
end

;; ====================================================== VISUALIZATION PROCEDURES ======================================================

to colorize
  ask patches [ set pcolor white ]
  ask followers [set color gray set hidden? true ]
  ask turtles [
    set color ifelse-value (protest-topic = 0) [0]
      [scale-color (item ((protest-topic - 1) mod length base-colors) base-colors) (item (protest-topic - 1) concerns) (0) (max-concern * 2) ]
    set shape ifelse-value (protest-status != "social")  ["circle"] ["circle 2"]
  ]
end


;; ===================================================== HELPING REPORTERS ======================================================

to-report increased-concern [cons top] ; cons is list of concerns, top is the topic of concern
  report replace-item (top - 1) cons min list max-concern ((item (top - 1) cons) + 1)
end

to-report random-select-topic [weights] ; from a list of n weights it select a number between 1 and n with probabilities proportional to weights
  report first rnd:weighted-one-of-list (map list (n-values length weights [i -> i + 1]) weights) [ [p] -> last p ]
end

to-report list-to-hist [li bins] ; tranforms a list of numbers li to list of counts of 0's, 1's, 2's, ..., bins's
  report map [i -> count-in-list li i] n-values (bins) [i -> i]
end

to-report count-in-list [li num]  ; counts how often the number num appears in the list li
  report length filter [i -> i = num] li
end

to-report random-binomial [n p] ; produce a random number between 0 and n with binomial distribution
  report sum n-values n [ifelse-value (random-float 1 < p) [1] [0]]
end

to-report eff-num-topics ; compute the effective number of protest-topics in the population (analog https://en.wikipedia.org/wiki/Effective_number_of_parties)
  let all-protest-topics list-to-hist [protest-topic] of turtles with [protest-topic != 0] num-topics
  report ifelse-value (sum all-protest-topics = 0)
    [num-topics]
    [1 / sum map [i -> (i / sum all-protest-topics) ^ 2] all-protest-topics]
end

;; ============================ EXAMPLES ==============================

to example [cntry rSeed p-mechanism t new-seed-after-setup?]
  baseline-network
  baseline-topics
  baseline-concern-thresh
  if cntry = "Germany" [ set num-topics 5 set threshold-level 0.7]
  set keep-network false
  set randomSeed? true
  set randomSeed rSeed
  set protest-mechanisms p-mechanism
  setup
  if new-seed-after-setup? [set randomSeed new-seed random-seed randomSeed]
  repeat t [go]
end

to baseline-network
  set following 5
  set friends 5
  set friends-network "random"
end

to baseline-topics
  set num-topics 9
  set max-concern 10
end

to baseline-concern-thresh
  set initial-concern-level 0.1
  set threshold-level 0.5
  set threshold-dispersion 0.2
end
@#$#@#$#@
GRAPHICS-WINDOW
370
110
898
639
-1
-1
8.525
1
10
1
1
1
0
0
0
1
-30
30
-30
30
1
1
1
ticks
30.0

BUTTON
305
35
360
68
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
370
70
425
103
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
35
300
68
population
population
5
1000
1000.0
5
1
NIL
HORIZONTAL

BUTTON
370
35
425
68
Go 20
repeat 20 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
505
195
538
threshold-level
threshold-level
0
1
0.7
0.05
1
NIL
HORIZONTAL

SLIDER
10
470
195
503
initial-concern-level
initial-concern-level
0
1
0.1
0.05
1
NIL
HORIZONTAL

PLOT
200
470
360
590
protest threshold
threshold
freq
0.0
10.0
0.0
10.0
true
false
"" "clear-plot\nset-plot-x-range 0 1.01"
PENS
"default" 0.025 1 -16777216 true "" "histogram [protest-threshold] of turtles"

TEXTBOX
10
445
305
463
Initial Concerns and Protest Thresholds
14
14.0
1

PLOT
905
495
1205
640
protest topics
topic
freq
0.0
11.0
0.0
10.0
true
false
"" "set-plot-x-range 0 (num-topics + 1)"
PENS
"default" 1.0 1 -16777216 true "" "histogram [protest-topic] of turtles with [protest-topic != 0]"

PLOT
905
255
1205
450
protest topics chosen
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"foreach (n-values num-topics [i -> i + 1]) [ x ->\n  create-temporary-plot-pen word \"top-\" x\n  set-plot-pen-color item ((x - 1) mod length base-colors) base-colors\n]" "foreach (n-values num-topics [i -> i + 1]) [ x -> \n  set-current-plot-pen word \"top-\" x\n  plot count turtles with [protest-topic = x]\n]"
PENS

SLIDER
10
395
110
428
num-topics
num-topics
1
20
5.0
1
1
NIL
HORIZONTAL

PLOT
905
40
1220
250
protesters
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" "set-plot-y-range 0 population"
PENS
"all" 1.0 0 -16777216 true "" "plot count turtles with [protest-status != \"no\"]"
"concern" 1.0 0 -817084 true "" "plot count turtles with [protest-status = \"concern\"]"
"social" 1.0 0 -13791810 true "" "plot count turtles with [protest-status = \"social\"]"

SLIDER
10
120
165
153
following
following
0
30
5.0
1
1
NIL
HORIZONTAL

SLIDER
10
155
165
188
friends
friends
0
30
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
10
80
210
120
Follower (directed) and \nFriends (reciprocal) Network
14
14.0
1

PLOT
170
120
360
240
Followers
# Follower
freq
0.0
10.0
0.0
10.0
true
false
"" "set-plot-x-range 0 1 + max [count in-follower-neighbors] of turtles"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count in-follower-neighbors] of turtles"

PLOT
170
240
360
360
Following
# Following
freq
0.0
10.0
0.0
10.0
true
false
"" "set-plot-x-range 0 1 + max [count in-follower-neighbors] of turtles"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count out-follower-neighbors] of turtles"

MONITOR
95
240
170
285
avg degree
mean [count in-follower-neighbors] of turtles
2
1
11

TEXTBOX
10
370
265
388
Protest Topics and Concerns\n
14
14.0
1

CHOOSER
10
190
165
235
friends-network
friends-network
"random" "ring" "cliques"
0

MONITOR
75
590
200
635
avgerage threshold
mean [protest-threshold] of turtles
3
1
11

SWITCH
575
35
725
68
concern-protest
concern-protest
0
1
-1000

SWITCH
730
35
885
68
social-activation
social-activation
0
1
-1000

SWITCH
595
70
780
103
social-media-concern
social-media-concern
0
1
-1000

MONITOR
1150
115
1250
160
frac. protest
count turtles with [protest-status != \"no\"] / count turtles
3
1
11

MONITOR
1150
205
1250
250
frac. social
count turtles with [protest-status = \"social\"] / count turtles
3
1
11

MONITOR
1150
160
1250
205
frac. concerned
count turtles with [protest-status = \"concern\"] / count turtles
3
1
11

MONITOR
1010
450
1205
495
effective number of topics
eff-num-topics
3
1
11

SLIDER
110
395
230
428
max-concern
max-concern
1
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
20
540
195
573
threshold-dispersion
threshold-dispersion
0
0.5
0.2
0.05
1
NIL
HORIZONTAL

TEXTBOX
10
10
175
30
Setup Parameters
18
14.0
1

TEXTBOX
370
10
675
30
Protest Mechanisms
18
14.0
1

TEXTBOX
910
10
1125
30
Output Measures
18
14.0
1

MONITOR
905
450
1010
495
not protesting
count turtles with [protest-topic = 0]
17
1
11

MONITOR
200
590
275
635
thresh < 0
count turtles with [protest-threshold < 0]
1
1
11

MONITOR
275
590
360
635
thresh > 1
count turtles with [protest-threshold >= 1]
17
1
11

CHOOSER
430
35
570
80
protest-mechanisms
protest-mechanisms
"manual" "sa" "cp" "cp-sa" "cp-smc" "cp-smc-sa"
5

SWITCH
225
70
360
103
keep-network
keep-network
1
1
-1000

SWITCH
1265
40
1410
73
randomSeed?
randomSeed?
0
1
-1000

INPUTBOX
1265
80
1410
140
randomSeed
17.0
1
0
Number

OUTPUT
1265
145
1410
200
11

TEXTBOX
1265
15
1475
35
Random Seed Settings
18
14.0
1

BUTTON
1220
420
1555
453
Example Germany
example \"Germany\" 17 \"cp-smc-sa\" 146 false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1220
295
1555
328
Example Iran
example \"Iran\" 16 \"cp-smc-sa\" 63 false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1220
455
1555
488
Example Germany without social activation
example \"Germany\" 17 \"cp-smc\" 146 false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1215
265
1365
286
Example Runs
18
14.0
1

BUTTON
1220
365
1555
398
Example Iran, new random seed after setup
example \"Iran\" 16 \"cp-smc-sa\" 100 true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1220
490
1555
523
Example Germany , new random seed after setup
example \"Germany\" 17 \"cp-smc-sa\" 146 true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1220
330
1555
363
Example Iran without social media concern
example \"Iran\" 16 \"cp-sa\" 63 false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This model accompanies the paper

**"On the fate of protests: Dynamics of activation and topic selection online and in the streets" 
Ahmadreza Asgharpourmasouleh, Masoud Fattahzadeh, Daniel Mayerhoffer, Jan Lorenz (2019)**

We model of the evolution of street protests in terms of individual concerns, social activation, topic selection and increasing concerns through topic propagation in social media. We only model the activation of protesters and the build-up of concerns and not the dampening and die-out of protests. Before specifying the details we describe the basic idea. 

## HOW IT WORKS

### Agents, follower network, thresholds, and concerns

The agents in the model are individuals represented by dots. They are connected in a static directed social network built upon setup. The network is use for layout but loinks are hidden to speed up visualization. The network can be interpreted as a follower network where an agent can read the social media posts of the other agents following but not necessarily vice versa. The follower network is created in two parts. First, a directed preferential attachment network is built, representing links to potential “celebrities” at various magnitudes of fame. This is built by successively creating agents where upon creation each follows a fixed number (`following`) of other already existing agents (or all other agents for the very first agents), where agents are selected with probabilities proportional to the number of current followers (plus one to give the newest nodes a non-zero probability to become selected). Second, a friends network is built. A friend-link is represented by a a reciprocal follower relations. The friends network is a random network, where every possible link is created with a probability selected such that the expected number of friends is a certain integer (`friends`).
   
As variables each agents has a `protest-threshold` which stays constant after initialization and a list `concerns` of length `num-topics` (the latter is a global parameter). The values in the list represent concerns about different topics. The concern values can change over time. Protest thresholds are real numbers. Any number larger than one represents an agent who will never join a protest and any value zero or less represents an agent who will always protest regardless of concern or protesting others. Concern values are integers including of zero but less or equal to `max-concern`. In a world with five topics, the concern vector [0 2 3 0 7] represents that the agent is mostly concerned about topic 5 and not at all concerned about topics 1 and 4. Two other dynamic variables of agents are the `protest-status` which is either “concern”, “social”, or “no” and the `protest-topic` on which they post a message in social media. The protest topic is zero if an agent is not protesting, or the index of the topic they chose for the protest. As this is chosen randomly with probabilities proportional to concerns, this is usually a topic on which they have a high concern value. The topic of protest represents a protest-related social-media post which can be read by followers. 
 
### Agents’ activities
On each tick (we can think of a day) agents do the following activities. 

#### Step 1. (All agents asked in random order.)
All agents which are not already concerned enough to protest read their `news-feed` and compute the fraction of the people they follow who protest. The `news-feed` is a list of the `protest-topic` or the not protest-related messages of all agents the agent follows. From the news-feed the agent can also extract the fraction of protesting people. 

#### Step 2. (All agents asked in random order) 
All agents decide if they join the protest. Based on the following steps each agent runs through.

##### Step 2.1. (Concern protest) 
An agent checks if a concern on at least one topic is larger than the individual protest threshold times the `max-concern`. If this is the case the agent sets the protest status to “concern”. Such an agent then selects the topic of concern from all the topics which are above the threshold randomly with probabilities proportional to concern values. 

##### Step 2.2. (Social media concern) 
Agents without concerns above the threshold will read their `news-feed` and select one topic of concern from this list at random. This can well be zero representing a message which is not protest related. In this case, nothing more happens. If it is a protest topic, the agent will increase the concern value on that topic by one but not above `max-concerns`.

##### Step 2.3. (Social activation) 
Agents without concerns above the threshold check if the fraction of people followed who protest is larger than the threshold. If this is the case the agent is socially activated and sets the `protest-status` to “social” otherwise to “no”. If the agent protests one of the topics is selected for the protest at random with probabilities proportional to the concern values even though the concerns themselves are not above threshold.

### Initial conditions and simulation time
The initialization of a simulation run (setup procedure) is done as follows. First, `population` agents are created. Then directed links are created using a preferential attachment generator. Further reciprocal friends links are created in a random network. Network generation is steered by the parameters `following` and `friends` which describe the desired average number of directed and reciprocal links an agent should have. Agents’ static protest thresholds are random numbers from a normal distribution with mean `threshold-level` and standard deviation `threshold-dispersion`. Agents’ dynamic `concerns` vectors are `topic-num` integers between zero and `max-concern`. The concern value on each topic is initialized as a binomial random number with probability `initial-concern-level`. This implies that the expected concern on each topic for each agent is `initial-concern-level` times `max-concern`. The three `protest-mechanisms` of `concern-protest` (CP), `social-media-concern` (SMC), and `social-activation` (SA) described above can all be independently switched “on” and “off”. All “on” specifies the full model. 


## HOW TO USE IT AND THINGS TO NOTICE

Click `setup` and `go`. First try the six example buttons. 

The Iran example shows the emergence of a topic diverse protest. With the second Iran button you can check that this protest would not take off without social media concerns. With the third Iran button you can check other realization for the same initial condition showing that there is variation in the effective numbe of topics and which topic becomes that mast selected one. 

The Germany example shows the emergence of a protest with one dominant topic which is not the one that is the initial forerunner. The second Germany button shows that social activation is essential for this over-taking. Use the third button to check different realizations with the same initial conditions. There is some variation, but most of the time the same topic becomes dominant.  


You may further explore how the three protest-mechanisms interplay. 
It turns out that only the five combinations CP, SA, CP-SA, CP-SMC, and CP-SMC-SA are sensible configurations to distinguish. Just SMC will never spark anyone to protest, and SMC-SA would fully coincide with SA with respect to the protest status of agents. A logical analysis of all model variants shows that an agent can only experience three types of transitions of her protest status:  “no” → “concern”, “no” → “social”, and “social” → “concern”. Therefore, the total number of protesters (genuinely concerned or socially activated) can only increase or stay constant. As already mentioned, the decline of a protests was deliberately not the aim of the modeling. Only when social media concern is switched on, the concerns of agents can increase. Thus, it is easy to see that in the CP regime the total number of protesters is reached after one time-step, and in the SA and CP-SA regime, the total number of protesters is reached when the number of protesters stays constant for one time-step. The chosen protest topics may change but the distribution for the probabilistic selection stays constant. When social media concern is switched on, every agent who is at some point following a protesting agent will successively increase her concern on at least one topic in the long run. Consequently, she will turn to a concerned protester unless she has a protest threshold above one which rules out any protest. Thus, in most configurations with social media concern, the society ends up with all agents with thresholds below one end up with protest status “concern”. Further on, the concerns of agents remain stable once they are concerned protesters. Therefore, also the distribution for the random selection of topics of concern in the whole society stabilizes once all protesters turn to be concerned protesters. We use these insights to define stopping rules for simulation runs. 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -1 true false 75 75 150

circle 3
false
0
Circle -7500403 true true 0 0 300
Rectangle -1 true false 120 45 180 255
Rectangle -1 true false 45 120 255 180

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Net 5 5, TopConcern 9 10, 0.1 X 0.2 protest-mechanisms" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>ticks</metric>
    <metric>eff-num-topics</metric>
    <metric>count turtles with [protest-status = "social"]</metric>
    <metric>count turtles with [protest-status = "concern"]</metric>
    <metric>count turtles with [protest-topic = 1]</metric>
    <metric>count turtles with [protest-topic = 2]</metric>
    <metric>count turtles with [protest-topic = 3]</metric>
    <metric>count turtles with [protest-topic = 4]</metric>
    <metric>count turtles with [protest-topic = 5]</metric>
    <metric>count turtles with [protest-topic = 6]</metric>
    <metric>count turtles with [protest-topic = 7]</metric>
    <metric>count turtles with [protest-topic = 8]</metric>
    <metric>count turtles with [protest-topic = 9]</metric>
    <metric>used-random-seed</metric>
    <enumeratedValueSet variable="initial-concern-level">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="threshold-level" first="0" step="0.05" last="0.8"/>
    <enumeratedValueSet variable="threshold-dispersion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protest-mechanisms">
      <value value="&quot;sa&quot;"/>
      <value value="&quot;cp&quot;"/>
      <value value="&quot;cp-sa&quot;"/>
      <value value="&quot;cp-smc&quot;"/>
      <value value="&quot;cp-smc-sa&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="following">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends-network">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-topics">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-concern">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-network">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size-as">
      <value value="&quot;number followers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-links">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Net 5 5, TopConcern 9 10, X 0.5 0.2 protest-mechanisms" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>ticks</metric>
    <metric>eff-num-topics</metric>
    <metric>count turtles with [protest-status = "social"]</metric>
    <metric>count turtles with [protest-status = "concern"]</metric>
    <metric>count turtles with [protest-topic = 1]</metric>
    <metric>count turtles with [protest-topic = 2]</metric>
    <metric>count turtles with [protest-topic = 3]</metric>
    <metric>count turtles with [protest-topic = 4]</metric>
    <metric>count turtles with [protest-topic = 5]</metric>
    <metric>count turtles with [protest-topic = 6]</metric>
    <metric>count turtles with [protest-topic = 7]</metric>
    <metric>count turtles with [protest-topic = 8]</metric>
    <metric>count turtles with [protest-topic = 9]</metric>
    <metric>used-random-seed</metric>
    <steppedValueSet variable="initial-concern-level" first="0" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="threshold-level">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-dispersion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="protest-mechanisms">
      <value value="&quot;cp-smc&quot;"/>
      <value value="&quot;cp-smc-sa&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="following">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends-network">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-topics">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-concern">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-network">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size-as">
      <value value="&quot;number followers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-links">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
