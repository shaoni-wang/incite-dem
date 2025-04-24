;extensions[matrix nw csv]
breed [humats humat]
breed [influencers influencer]
undirected-link-breed [connections connection]  ; connections among citizens
directed-link-breed [relations relation]        ; relations between citizens and authorites

globals [
  A-supporters
  B-supporters
  AB-supporters
  A-value+       ; opponents with satisfaction
  A-value-       ; opponents with dissatisfaction
  A-value
  B-value+
  B-value-
  B-value
  AB-value+
  AB-value-
  AB-value
  sr-supporters
  sr-opponents
  satisfaction-rate
  support-rate

  group-number
  polarisation
  setup?         ; being setted up or not
  norm-influence
  num-authority
  Thre_asse
  Thre_conf

]

influencers-own [; infuencers or authorities
  influence      ; for now random < 0 ; 1 > the higher, the less-or-more-vocalful
  endorsement    ; endorsing an alternative
]

humats-own [
  behaviour
  ses                  ; socio-economic status, the less-or-more-vocalful, for now random < 0 ; 1 > the higher,
  group-id
  assertiveness         ; relates to self-assertiveness
  conformity

;;;dissonance-related variables;;; ;variables calculated for all BAs;
  experiential-importance
  social-importance
  values-importance
  non-social-importance

  experiential-satisfaction-A
  social-satisfaction-A
  values-satisfaction-A
  non-social-evaluation-A

  experiential-satisfaction-B
  social-satisfaction-B
  values-satisfaction-B
  non-social-evaluation-B

  experiential-evaluation-A ; evaluation of A (behavioural alternative i) with respect to experiential group of needs for HUMAT j <-1;1>
  social-evaluation-A       ; evaluation of A (behavioural alternative i) with respect to social group of needs for HUMAT j <-1;1>
  values-evaluation-A       ; evaluation of A (behavioural alternative i) with respect to values for HUMAT j <-1;1>
  experiential-evaluation-B ; evaluation of B (behavioural alternative ~i) with respect to experiential group of needs for HUMAT j <-1;1>
  social-evaluation-B       ; evaluation of B (behavioural alternative ~i) with respect to social group of needs for HUMAT j <-1;1>
  values-evaluation-B       ; evaluation of B (behavioural alternative ~i) with respect to values for HUMAT j <-1;1>

  evaluations-list-A
  evaluations-list-B
  evaluations-A
  evaluations-B
  satisfaction-A
  satisfaction-B
  satisfaction              ; the satisfaction of the decision
  support                   ; the degreen of the support
  ind-opinion               ; individual-opinion
  satisfying-A              ; the sum of satisfying evaluations of A over three groups of needs <0;1,5>
  satisfying-B              ; the sum of satisfying evaluations of B over three groups of needs <0;1,5>
  dissatisfying-A           ; the sum of dissatisfying evaluations of A <0;1,5>
  dissatisfying-B           ; the sum of dissatisfying evaluations of B over three groups of needs <0;1,5>

  dissonance-A              ; the level of cognitive dissonance a behavioural alternative i (A) evokes in HUMAT j at time tn [Dij tn] <0;1>
  dissonance-B              ; the level of cognitive dissonance a behavioural alternative i (B) evokes in HUMAT j at time tn [Dij tn] <0;1>
  dissonance-tolerance      ; individual difference in tolerating dissonances before they evoke dissonance reduction strategies [Tj] normal trunc distribution with mean = 0.5, sd = 0.14 trunc to values <0;1>, this is the threshold determining if a reduction strategy is forgetting/distraction or if action has to be taken
  dissonance-strength-A     ; individually perceived strength of cognitive dissonance a behavioural alternative i (A) evokes in HUMAT j at time tn [Fij tn]; F because it's a fraction of maximum dissonance HUMAT j can possibly experience <0;1>
  dissonance-strength-B     ; individually perceived strength of cognitive dissonance a behavioural alternative i (B) evokes in HUMAT j at time tn [Fij tn]; F because it's a fraction of maximum dissonance HUMAT j can possibly experience <0;1>
  dissonance-strength       ; individually perceived strength of cognitive dissonance a chosen behavioural alternative evokes in HUMAT j at time tn [Fij tn]; either dissonance-strenght-A or dissonance-strength-B, depending on chosen behavioural alternative

  ;variables only calculated for the chosen BA, not for all BAs;
  experiential-dilemma?     ; the existence of an experiential dilemma e a chosen behavioural alternative i evokes in HUMAT j at time tn [Dije tn = {0,1}, where 0 donotes no dilemma and 1 denotes existence of dilemma]
  social-dilemma?           ; the existence of an social dilemma s a chosen behavioural alternative i evokes in HUMAT j at time tn [Dije tn = {0,1}, where 0 donotes no dilemma and 1 denotes existence of dilemma]
  values-dilemma?           ; the existence of an values dilemma v a chosen behavioural alternative i evokes in HUMAT j at time tn [Dije tn = {0,1}, where 0 donotes no dilemma and 1 denotes existence of dilemma]

;;;alter-representation variables;;;
  self-attri-list           ; the list contains individual properties
  alter-representation-list
  inquiring-list
  signaling-list
  inquired-humat            ; the list belongs to ego and contains information about the alter who the ego inquires with
  ;inquiring-humat           ; the list belongs to an inquired alter and contains information about the ego who is inquiring
  signaled-humat            ; the list belongs to ego and contains information about the alter who the ego signals to
  signaling-humat           ; the list belongs to a signaled alter and contains information about the ego who is signaling

  inquiring?                ; boolean positive [1] if the ego is inquiring at a given tick
  inquired?
  #inquiring                ; the number of times humat inquires with alters
  #inquired                 ; the number of times humat was inquired with by egos
  signaling?                ; boolean positive [1] if the ego is signaling at a given tick
  signaled?
  #signaling                ; the number of times humat signals to alters
  #signaled                 ; the number of times humat was signaled to

;;;;interaction record
  con-frequency             ; the frequence of interaction between the humat and (connected) others
]

;;;;;;;;;;;;;;;;;;
;;; Procedures ;;;
;;;;;;;;;;;;;;;;;;
to reset
  clear-all
  reset-ticks
  resize-world 0 200 0 200
  ask patches [set pcolor white]
  set group-number 1
  set setup? false
  set num-authority 0
  set Thre_asse 0.5
  set Thre_conf 0.5
end


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the community ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
to add-agents
 ifelse group-number  > num-groups
   [ user-message word (group-number - 1) " groups have been created!"]
   [ make-HUMATS
     to-show-labels?
   ]
  set norm-influence 0.1
end

to make-HUMATS
   random-seed 100                                        ; make the random process determined
   create-humats num-agents
    [
     set group-id group-number
;     set against-or-for-change 0.6
;     set less-or-more-vocal 0.6
;     set self-assertiveness 0.2

     set dissonance-tolerance random-normal-trunc 0.5 0.5 0 1
     set assertiveness random-normal-trunc self-assertiveness 0.05 0 1
     set ses random-normal-trunc less-or-more-vocal 0.05 0 1
     set size 4 * (less-or-more-vocal + 2)
     set inquired? 0
     set signaled? 0
     set label group-id
     set label-color black
    ]

  output-print (word "GROUP " group-number ":")
  output-print word "num-agents = " num-agents
  output-print word "less-or-more-vocal ≈ " less-or-more-vocal
  output-print word "against-or-for-change ≈ " against-or-for-change
  output-print "+++++++++++++++++++++++++"

  set group-number group-number + 1
  set-noun-social-need
  set-intra-networks                                      ; links within the same group
  set-social-satisfactions

  ask humats with [group-id = group-number - 1] [
   evaluate-satisfaction-support                          ; evaluate the support and satisfaction
   position-humats
   ]
end

to to-show-labels?
  ask humats [
    ifelse show-labels?
       [
        set label group-id
        set label-color black
        ]
       [set label ""]
   ]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
to setup-community
  clear-all-plots
  reset-ticks
  set setup? true

  ;policy_strategy
  to-show-labels?
  set-inter-networks
  set-social-satisfactions
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Social networks ;;;
;;;;;;;;;;;;;;;;;;;;;;;
to set-intra-networks                                 ; within the groups
 if num-agents > 1
  [ask HUMATS with [group-id = group-number - 1] [
    let topological-neighbours other HUMATS with [group-id = group-number - 1]

    let #topological-neighbours count topological-neighbours

    create-connections-with n-of round (0.2 * #topological-neighbours) topological-neighbours
    if count connection-neighbors = 0
      [create-connection-with min-one-of other HUMATS [distance myself]]
     ]
   ]
  ask connections
    [
     set color 7
     set thickness 0.01
     ]

  ifelse show-links?
    [ ask connections [set hidden? false]]
    [ ask connections [set hidden? true]]
end

;to set-intra-networks                                 ; within the groups
; if Num-Agents > 1
;  [ask HUMATS with [group-id = group-number - 1] [
;    let neighbours other HUMATS with [group-id = group-number - 1]
;
;    let #neighbours count neighbours
;    let avg-connections 5
;
;    if #neighbours < 5 [set avg-connections 2 * #neighbours]
;    create-connections-with n-of round (avg-connections / 2) neighbours
;    if count connection-neighbors = 0
;      [create-connection-with min-one-of other HUMATS [distance myself]]
;     ]
;   ]
;
;  ask connections
;    [ set color 7
;     set thickness 0.01
;     ]
;
;  ifelse show-links?
;    [ ask connections [set hidden? false]]
;    [ ask connections [set hidden? true]]
;end


to set-inter-networks                            ; links between the groups
 ask HUMATS [
    let self-group-id group-id
    let topological-neighbours other HUMATS with [group-id != self-group-id]
    let #topological-neighbours count topological-neighbours

    create-connections-with n-of round (0.1 * #topological-neighbours) topological-neighbours
             [
               set color 7
               set shape "inter-groups"
              ]
    set con-frequency n-values count humats [0]  ; set-up the list of connections of humat and other humats

    ]

    ifelse show-links?
    [ask relations [set hidden? false]
     ask connections [set hidden? false]
    ]
    [ask relations [set hidden? true]
     ask connections [set hidden? true]
    ]
end

;to set-inter-networks                            ; links between the groups
; ask HUMATS [
;    let self-group-id group-id
;    let topological-neighbours other HUMATS with [group-id != self-group-id]
;    let #neighbours count topological-neighbours
;
;    let avg-connections 5
;    if #neighbours < 5 [set avg-connections 2 * #neighbours]
;
;    create-connections-with n-of round (avg-connections / 4) topological-neighbours  ;round (0.1 * #topological-neighbours) topological-neighbours
;             [ set link-color 7
;               set color link-color
;               set thickness  0.01
;               set shape "inter-groups"
;              ]
;    set con-frequency n-values count humats [0]  ; set-up the list of connections of humat and other humats
;
;    ;;each individual only follows one authority
;    let rand-follow 0.8
;    let num-influence []
;    if num-authority > 0
;      [ ask influencers [
;         set num-influence lput [who] of self num-influence]
;
;        if random-float 1 < rand-follow
;        [let target influencer one-of num-influence
;          create-relation-to target
;             [set color [color] of target + 3.5
;              set thickness  0.01
;           ]
;         ]
;       ]
;    ]
;
;    ifelse show-links?
;    [ask relations [set hidden? false]
;     ask connections [set hidden? false]
;    ]
;    [ask relations [set hidden? true]
;     ask connections [set hidden? true]
;    ]
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Needs-and-satisfactions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to set-noun-social-need
;  HUMATS set their initial behavior with the esclusion of social influence, i.e. on the basis their experiential and value satisfactions
  ask HUMATS with [group-id = group-number - 1][
    ; set importances
    set experiential-importance random-normal-trunc 0.5 0.5 0 1 ;0.5
    set values-importance random-normal-trunc 0.5 0.5 0 1
    set social-importance norm-influence ;random-normal-trunc norm-influence 0.1 0 1

    if (assertiveness > 0) [ set social-importance  (social-importance / (assertiveness * 7)) ]

    ;either opposing or supporting,eg. A opposing the plan, B supporting the plan
    ifelse against-or-for-change < 0.5 ; smaller against-or-for-change, stonger opposing, less-or-more-vocal
      [ let against-value normalized-min-max (1 - against-or-for-change) 0.5 1 0 1
        set experiential-satisfaction-A random-normal-trunc against-value 0.5 -1 1  ; A opposing the plan， ((4 * against-value + less-or-more-vocal)/ 5)
        set values-satisfaction-A random-normal-trunc against-value 0.5 -1 1
        set experiential-satisfaction-B random-normal-trunc 0 0.5 -1 1
        set values-satisfaction-B random-normal-trunc 0 0.5 -1 1
      ]
      [ let for-value normalized-min-max against-or-for-change 0.5 1 0 1
        set experiential-satisfaction-A random-normal-trunc 0 0.5 -1 1
        set values-satisfaction-A random-normal-trunc 0 0.5 -1 1
        set experiential-satisfaction-B random-normal-trunc for-value 0.5 -1 1  ; B supporting the plan
        set values-satisfaction-B random-normal-trunc for-value 0.5 -1 1
          ]

    ; set evaluations = importances * satisfactions ; excluding social dimension
    set experiential-evaluation-A experiential-importance * experiential-satisfaction-A
    set values-evaluation-A values-importance * values-satisfaction-A
    set experiential-evaluation-B experiential-importance * experiential-satisfaction-B
    set values-evaluation-B values-importance * values-satisfaction-B

    ; Calculate non-social satisfaction
    set evaluations-list-A (list (experiential-evaluation-A) (values-evaluation-A))
    set evaluations-list-B (list (experiential-evaluation-B) (values-evaluation-B))
    set non-social-evaluation-A precision (mean evaluations-list-A) 4   ;[-1,1]
    set non-social-evaluation-B precision (mean evaluations-list-B ) 4

    ;;initial opinion, chosen of the basis of experiential needs and values; if similar eveluation from both BAs, choose randomly
    ifelse non-social-evaluation-A < non-social-evaluation-B
           [ set behaviour "B"]
           [ set behaviour "A"]
    ]
end

to set-social-satisfactions
; in step 2, HUMATS add the social satisfaction to the calculation of satisfactions from various behavioural alternatives
  ask HUMATS [
      set self-attri-list (list
        [who] of self                              ;item 0 who
        [behaviour] of self                        ;item 1 behaviour ; to do: as known for observable and as guessed for non-observable behaviours
        [experiential-importance] of self          ;item 2
        [social-importance] of self                ;item 3
        [values-importance] of self                ;item 4
        [experiential-satisfaction-A] of self      ;item 5
        [social-satisfaction-A] of self            ;item 6
        [values-satisfaction-A] of self            ;item 7
        [experiential-satisfaction-B] of self      ;item 8
        [social-satisfaction-B] of self            ;item 9
        [values-satisfaction-B] of self            ;item 10
        )

    ; STEP 2 based on cognition, not on interaction
    ; create alter representation lists for each alter  ; to do:if behaviours unobservable - here add the guessing parameter
    set-update-alter-representations
    alter-similarity
    calculate-dissonance
    calculate-dilemmas
 ]
end

to set-update-alter-representations
  let working-list n-values 5 [0]
  ask HUMATS [
    set alter-representation-list []
    foreach sort connection-neighbors [x ->
      let working-list1 ( list
        [who] of x                              ;item 0 who
        inquired?                               ;item 1 inquired? 0 for not inquired with, 1 for inquired with already
        signaled?                               ;item 2 signaled? 0 for not signaled to, 1 for signaled to already
        [behaviour] of x                        ;item 3 behaviour ; to do: as known for observable and as guessed for non-observable behaviours
        same-BA? behaviour [behaviour] of x     ;item 4 1 for same behaviour; 0 for different behaviour ; used for inquiring
        ifelse-value (same-BA? behaviour [behaviour] of x  = 1) [0] [1] ; item 5  1 for different behavior, used for signaling
        )

     ;if inquiring? = 1     ;;defaut or inquiring? = 1
        set working-list ( list
          need-similarity experiential-evaluation-A [experiential-evaluation-A] of x  experiential-importance [experiential-importance] of x  ;item 0 similarity-A-experiential-importance
          need-similarity values-evaluation-A [values-evaluation-A] of x  values-importance [values-importance] of x                            ;item 1 simiarity-A-values-importance
          need-similarity experiential-evaluation-B [experiential-evaluation-B] of x  experiential-importance [experiential-importance] of x  ;item 2 similarity-B-experiential-importance - similarity between the importance of needs; only > 0 if the given BA satisfies that group of needs in a similar direction for the alter and for the ego
          need-similarity values-evaluation-B [values-evaluation-B] of x  values-importance [values-importance] of x                            ;item 3 similarity-B-values-importance
          relative-aspiration [ses] of x ses      ;item 4 inquiring, relative-aspiration-influenced-ego - relative social appeal/status (aspiration characteristic)
      )

      if signaling? = 1     ;;for signaling
        [ set working-list ( list
          need-similarity [experiential-evaluation-A] of x experiential-evaluation-A [experiential-importance] of x experiential-importance
          need-similarity [values-evaluation-A] of x values-evaluation-A  [values-importance] of x values-importance
          need-similarity [experiential-evaluation-B] of x experiential-evaluation-B [experiential-importance] of x experiential-importance
          need-similarity [values-evaluation-B] of x values-evaluation-B  [values-importance] of x values-importance
          relative-aspiration ses [ses] of x
          )
         ]

        let item11 (item 0 working-list * item 4 working-list)  ;item 11 persuasion-A-experiential,item 11 of the final working-list
        let item12 (item 1 working-list * item 4 working-list)  ;item 12 persuasion-A-values
        let item13 (item 2 working-list * item 4 working-list)  ;item 13 persuasion-B-experiential
        let item14 (item 3 working-list * item 4 working-list)  ;item 14 persuasion-B-values
        let item15 (item11 + item12 + item13 + item14)          ;item 15 overall-persusasion
        let working-list2 (list item11 item12 item13 item14 item15)
        set working-list   sentence working-list1 working-list
        set working-list   sentence working-list working-list2
        set alter-representation-list lput working-list alter-representation-list
      ]
  ]
end


to alter-similarity  ;;relates to social need
    ; go through alter-representation-list and count the alters, who behave similarily and dissimilarily
    let #similar 0
    let #dissimilar 0
    foreach alter-representation-list [x -> if item 4 x = 1 [set  #similar  #similar + 1]]
    foreach alter-representation-list [x -> if item 5 x = 1 [set  #dissimilar  #dissimilar + 1]]

    let #alters count link-neighbors
    let %similar 0
    let %dissimilar 0
    ifelse #alters > 0
      [ set %similar #similar / #alters
        set %dissimilar #dissimilar / #alters
        ]
      [
        set %similar 0
        set %dissimilar 0
       ]

    ; set social dimension: social satisfaction from BAs, evaluations of BAs
    if behaviour = "A"
      [
        set social-satisfaction-A normalized-min-max %similar 0 1 -1 1
        set social-satisfaction-B normalized-min-max %dissimilar 0 1 -1 1
       ]
    if behaviour = "B"
      [
        set social-satisfaction-A normalized-min-max %dissimilar 0 1 -1 1
        set social-satisfaction-B normalized-min-max %similar 0 1 -1 1
       ]

    ; social influence from here
    set social-evaluation-A social-importance * social-satisfaction-A
    set social-evaluation-B social-importance * social-satisfaction-B
    set evaluations-list-A (list (experiential-evaluation-A) (social-evaluation-A) (values-evaluation-A))
    set evaluations-list-B (list (experiential-evaluation-B) (social-evaluation-B) (values-evaluation-B))

    ; set final satisfactions from BAs at setup stage
    set evaluations-A precision (mean evaluations-list-A) 4
    set evaluations-B precision (mean evaluations-list-B) 4
    set satisfaction-A mean (list experiential-satisfaction-A social-satisfaction-A values-satisfaction-A)
    set satisfaction-B mean (list experiential-satisfaction-B social-satisfaction-B values-satisfaction-B)
end

to calculate-dissonance
    set satisfying-A satisfying-status-BA evaluations-list-A
    set dissatisfying-A dissatisfying-status-BA evaluations-list-A
    set satisfying-B satisfying-status-BA evaluations-list-B
    set dissatisfying-B dissatisfying-status-BA evaluations-list-B

    set dissonance-A dissonance-status-A satisfying-A dissatisfying-A
    set dissonance-B dissonance-status-B satisfying-B dissatisfying-B

   ; calculating the need for dissonance reduction - a BA invokes the need to reduce dissonance if the level of dissonance for BA exceeds the dissonance-threshold
    set dissonance-strength-A (dissonance-A - dissonance-tolerance) / (1 - dissonance-tolerance)
    if dissonance-strength-A < 0
      [set dissonance-strength-A 0] ; if the dissonance level a behavioural alternative i (A) [Dij tn] does not exceed the individual tolerance threshold of HUMAT j [Tj], HUMAT j does not experience any dissonance: [Dij tn] < [Tj] -> [Fij tn] = 0

    set dissonance-strength-B  (dissonance-B - dissonance-tolerance) / (1 - dissonance-tolerance)
    if dissonance-strength-B < 0    ;the greater the dissonance, the more unsatisfied with the choice.
      [set dissonance-strength-B 0] ;when dissonance approaches to 0 or smaller than the dissonance-tolerance, the more satisfied with the choice
end

to calculate-dilemmas
   ifelse behaviour = "A"
    [
     if (experiential-evaluation-A > 0 and social-evaluation-A < 0 and values-evaluation-A < 0)  or
        (experiential-evaluation-A <= 0 and social-evaluation-A >= 0 and values-evaluation-A >= 0)
        [set experiential-dilemma? 1]
     if (social-evaluation-A > 0 and experiential-evaluation-A < 0 and values-evaluation-A < 0)  or
        (social-evaluation-A <= 0 and experiential-evaluation-A >= 0 and values-evaluation-A >= 0)
        [set social-dilemma? 1]
     if (values-evaluation-A > 0 and experiential-evaluation-A < 0 and social-evaluation-A < 0)  or
        (values-evaluation-A <= 0 and experiential-evaluation-A >= 0 and social-evaluation-A >= 0)
        [set values-dilemma? 1]
    ]
    [
     if (experiential-evaluation-B > 0 and social-evaluation-B < 0 and values-evaluation-B < 0)  or
        (experiential-evaluation-B <= 0 and social-evaluation-B >= 0 and values-evaluation-B >= 0)
        [set experiential-dilemma? 1]
     if (social-evaluation-B > 0 and experiential-evaluation-B < 0 and values-evaluation-B < 0)  or
        (social-evaluation-B <= 0 and experiential-evaluation-B >= 0 and values-evaluation-B >= 0)
        [set social-dilemma? 1]
     if (values-evaluation-B > 0 and experiential-evaluation-B < 0 and social-evaluation-B < 0)  or
        (values-evaluation-B <= 0 and experiential-evaluation-B >= 0 and social-evaluation-B >= 0)
        [set values-dilemma? 1]
    ]
end

to evaluate-satisfaction-support
 if ticks = 0    ; initial opinon
   [ if num-authority = 0
       [ let abs-eveluatAB abs (evaluations-A - evaluations-B)
         ifelse abs-eveluatAB <= 0.02
            [ set behaviour "AB"
             ]
            [ ifelse evaluations-A < evaluations-B
                [ set behaviour "B" ]
                [ set behaviour "A" ]
             ]
        ]
    ]

  let abs-satisfaction 0
  if behaviour = "A"   ;;oppose the plan
    [ set color magenta
      set dissonance-strength dissonance-strength-A
      set support normalized-min-max non-social-evaluation-A -1 1 0.01 5
      set satisfaction normalized-min-max satisfaction-A -1 1 1 9
      set abs-satisfaction satisfaction-A
     ]

  if behaviour = "AB"   ;;neutral point
     [set color gray
      set dissonance-strength  mean (list dissonance-strength-A dissonance-strength-B)
      set support normalized-min-max mean (list non-social-evaluation-A non-social-evaluation-B) -1 1 4 6 ;4.5 5.5
      set satisfaction normalized-min-max mean (list satisfaction-A satisfaction-B) -1 1 1 9
      set abs-satisfaction mean (list satisfaction-A satisfaction-B)
      ]

  if behaviour = "B"    ;;support the plan
     [set color green
      set dissonance-strength dissonance-strength-B
      set support normalized-min-max non-social-evaluation-B -1 1 5 10; 5.5 10
      set satisfaction normalized-min-max satisfaction-B -1 1 1 9
      set abs-satisfaction satisfaction-B
      ]

   let smile word "smile " group-id
   let sad word "sad " group-id
   let neutral word "neutral " group-id
   set abs-satisfaction abs abs-satisfaction
   set ind-opinion support / 10

  ifelse abs-satisfaction <= 0.05      ;; middle/moderate area, three types of face
     [set shape neutral]
     [ifelse satisfaction > 5          ;; nomalized satisfaction
       [set shape smile]
       [set shape sad ]
      ]
end

to choose-action                       ;; HUMAT-oriented
   random-seed 200
   let num-A 0
   let num-B 0
   let num-AB 0
   ask connection-neighbors [
      if behaviour = "A" [set num-A num-A + 1]
      if behaviour = "B" [set num-B num-B + 1]
      if behaviour = "AB" [set num-AB num-AB + 1]
     ]

   if assertiveness >= Thre_asse and social-importance <= Thre_conf   ;; assertive
       [set behaviour behaviour]

   if assertiveness < Thre_asse and social-importance > Thre_conf     ;; follow norms
       [;;follow norms
          ifelse num-A >= num-B
            [ifelse num-B >= num-AB
              [set behaviour "A"]
              [ifelse num-A >= num-AB
                 [set behaviour "A"]
                 [set behaviour "AB"]
               ]
             ]
            [ifelse num-B <= num-AB
              [set behaviour "AB"]
              [set behaviour "B" ]
             ]
        ]

    ;; comprimise or conflict, need to compare
    if (assertiveness > Thre_asse and social-importance > Thre_conf) or
       (assertiveness < Thre_asse and social-importance < Thre_conf) [
         comparison-needed
    ]
end

to comparison-needed
   ifelse further-comparison-needed? evaluations-A evaluations-B 2   ; The BA comparison dimensions include: 1.overall satisfaction;
    [compare-dissonances]                                           ; 2.dissonance level;3. satisfaction on experiential need
    [ifelse evaluations-A <= evaluations-B                          ; 4.neutral choice.
        [ set behaviour "B" ]
        [ set behaviour "A" ]
     ]
end


to compare-dissonances                                                       ; HUMAT-oriented
  ifelse further-comparison-needed? dissonance-A dissonance-B 2
    [ compare-experiential-needs ]
    [ ifelse dissonance-A <= dissonance-B
        [ set behaviour "A" ]
        [ set behaviour "B" ]
     ]
end

to compare-experiential-needs                                                ; HUMAT-oriented
  ifelse further-comparison-needed?  experiential-satisfaction-A  experiential-satisfaction-B 1
    [ set behaviour "AB"

  ]                                                                          ; neutral choice
    [ ifelse experiential-satisfaction-A < experiential-satisfaction-B
        [ set behaviour "B" ]
        [ set behaviour "A" ]
     ]
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Go Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;
to go
ifelse not setup?
  [ user-message "SETUP-COMMUNITY at First!"]
  [ every 0.1                                                                 ; set the speed of running
    [
      if ticks >= time-voting [ stop ]                                        ; time for ending the voting process
      intervention-policy
      update-humats
      inquire
      signal
      eveluation
      tick
    ]
   ]
end

to intervention-policy
 if intervention? [
    if (ticks = 50) ;and (ticks mod 5 = 0)
      [
        ask humats with [group-id = 1]
        [ ;;start to talk with people from the outside
          set norm-influence 0.1

          ;;mitigate the resistance
          set experiential-satisfaction-A experiential-satisfaction-A - 1
          set values-satisfaction-A values-satisfaction-A - 1
          if experiential-satisfaction-A < -1 [set experiential-satisfaction-A -1]
          if values-satisfaction-A < -1 [set values-satisfaction-A -1]

          ;; enhance the support
;          set experiential-satisfaction-B experiential-satisfaction-B + 0.1
;          set values-satisfaction-B values-satisfaction-B + 0.1
;          if experiential-satisfaction-B > 1 [set experiential-satisfaction-B 1]
;          if values-satisfaction-B > 1 [set values-satisfaction-B 1]

          set-social-satisfactions
          set-update-alter-representations
          update-experiential-and-values-evaluations
         ]
       ]
    ]
end


to inquire
  ;inquire strategy - seek information in social network to reduce cognitive dissonance via altering EGO’s knowledge structures
  ;during inquiring information flows uni-directionally from the alter, who was giving advice to the ego, who was asking for advice
  ;inquired-humat - the list belongs to ego and contains information about the alter who the ego inquires with
  ;inquiring-humat - the list belongs to an inquired alter and contains information about the ego who is inquiring

  ;update ego's representation of the inquired alter
  ask humats [
    set inquired-humat []
    if dissonance-strength > 0 and (values-dilemma? = 1 or experiential-dilemma? = 1)
      [set inquiring? 1
       set-update-alter-representations
       set inquiring-list sort-list alter-representation-list 1 4 15
       set #inquiring #inquiring + inquiring?
       set inquired-humat item 0 inquiring-list                ;; the humat being inquired

       ;update satisfactions of ego, update evaluations of experiential-and-values, and dissonance
       set experiential-satisfaction-A new-need-satisfaction-inquiring experiential-satisfaction-A 11 5  ; none persuasive * ego's satisfaction + persuasive * alter's satisfaction
       set values-satisfaction-A new-need-satisfaction-inquiring values-satisfaction-A 12 7
       set experiential-satisfaction-B new-need-satisfaction-inquiring experiential-satisfaction-B 13 8
       set values-satisfaction-B new-need-satisfaction-inquiring values-satisfaction-B 14 10

       ;update experiential-and-values-evaluations and the interaction frequency among humats, the side of being inquired
       update-experiential-and-values-evaluations
       update-dissonances
       set con-frequency replace-item (item 0 inquired-humat) con-frequency ((item (item 0 inquired-humat) con-frequency) + 2)

       ask humat item 0 inquired-humat [
          set inquired? 1             ;the fact that the humat was inquired
          set con-frequency replace-item ([who] of myself) con-frequency ((item ([who] of myself) con-frequency) + 2)      ;update the interaction frequency among humats, the side of being inquring
          set #inquired #inquired + 1
        ]
      ]
    update-networks
  ]
end

to signal
  ;signaling strategy - seek information in social network to reduce cognitive dissonance via altering ALTER’s knowledge structures
  ;during signaling information flows uni-directionally from the ego, who was giving advice to the alter, who was made to listed
  ;signaled-humat - the list belongs to ego and contains information about the alter who the ego signals to
  ;signaling-humat - the list belongs to a signaled alter and contains information about the ego who is signaling

 ask humats [
 ;   set signaling-list sort-list alter-representation-list 2 5 30
    set signaled-humat []
    set signaling-humat []
    if dissonance-strength > 0 and social-dilemma? = 1
      [
        set signaling? 1
        set-update-alter-representations
        set signaling-list sort-list alter-representation-list 2 5 15

        set signaled-humat item 0 signaling-list         ; ego's representation of an alter as a temporary list

        ask humat item 0 signaled-humat [                ; update alter's representation of ego, signaling-humat
          set signaled? 1                                ; representing the fact that alter was signaled to
          set #signaled #signaled + 1
          foreach alter-representation-list
            [ x ->
              if item 0 x = [who] of myself
              [set signaling-humat item position x alter-representation-list alter-representation-list]
             ]

          ; seting new experiential and values satisfaction
           set experiential-satisfaction-A new-need-satisfaction-signaling experiential-satisfaction-A 11
           set values-satisfaction-A new-need-satisfaction-signaling values-satisfaction-A 12
           set experiential-satisfaction-B new-need-satisfaction-signaling experiential-satisfaction-B 13
           set values-satisfaction-B new-need-satisfaction-signaling values-satisfaction-B 14

           ;updating evaluations and dissonances,update the interaction frequency among humats, side of the humat being signaled
           update-experiential-and-values-evaluations

           update-dissonances
           set con-frequency replace-item ([who] of myself) con-frequency ((item ([who] of myself) con-frequency) + 2)
          ]

       set #signaling #signaling + signaling?
       set con-frequency replace-item (item 0 signaled-humat) con-frequency ((item (item 0 signaled-humat) con-frequency) + 2)
      ]
     update-networks
  ]
end


to update-dissonances
 ; each tick, all humats recalculate their social satisfactions and final satisfactions, and dissonances, not only the one that changed his mind
 ; update social dimension: social satisfaction from BAs, evaluations of BAs
   alter-similarity
   calculate-dissonance
   calculate-dilemmas
end


to update-humats
 if (ticks > 0) and (ticks mod 10 = 0) and (norm-influence <= 1)
   [set norm-influence norm-influence + 0.1]      ; the norm influence is growing in the community

 ask humats [
    set con-frequency (map [x -> x - 1 ] con-frequency)                          ; update the overall connections-frequency at each tick
    set social-importance norm-influence ;random-normal-trunc norm-influence  0.1 0 1

    if(assertiveness > 0) [ set social-importance  (social-importance / (assertiveness * 7)) ]
    choose-action
    evaluate-satisfaction-support
    position-humats
   ]
end

;to position-humats
;  setxy (support * 18 + random-float 2)  (20 * satisfaction)
;end

to position-humats
  setxy (support * 18 + random-float 2)  (17 * satisfaction + random-float 3)
end


to update-networks
  ask humats[
    let my-connections-with-other-humats my-connections with [[other-end] of myself != nobody]
    let other-humats [other-end] of my-connections-with-other-humats
    set other-humats turtle-set other-humats         ; should distinguish intra or inter-group links from here!!!
    let other-humats-ids sort [who] of other-humats
    let con-frequency-temp con-frequency

    let id1 [who] of self
    let groupID1 [group-id] of self

    let i 0
    while [ i < length other-humats-ids]
      [let id2 item i other-humats-ids
       let groupID2 [group-id] of (humat id2)
       ask connection id1 id2 [
         let greyscale item id2 con-frequency-temp
         let time-fading 0
         ifelse groupID1 = groupID2
            [set time-fading  time-voting * (1 + 0.05)]    ; 0.1
            [set time-fading  time-voting * (1 - 0.2)]

         ifelse greyscale >= 0                                                    ; existing interactions
            [ set color 7 / e ^ (0.2 * greyscale)
             ]
            [
             ifelse greyscale > (- time-fading)                                   ; (- time-for-vote * 0.8)
               [set color 3.9 / e ^ (0.2 * (greyscale + time-fading)) + 6]   ; link-color is 5, if no(greyscale = 0) interaction
               [set color 9.9
                set hidden? true
                ]
             ]
        ]
       set i i + 1
       ]
     ]
end

to update-experiential-and-values-evaluations
  ; update evaluations = importances * satisfactions ; excluding social dimension
  set experiential-evaluation-A experiential-importance * experiential-satisfaction-A
  set values-evaluation-A values-importance * values-satisfaction-A
  set experiential-evaluation-B experiential-importance * experiential-satisfaction-B
  set values-evaluation-B values-importance * values-satisfaction-B

  ;;non-social satisfaction for the support
  set evaluations-list-A (list (experiential-evaluation-A) (values-evaluation-A))
  set evaluations-list-B (list (experiential-evaluation-B) (values-evaluation-B))
  set non-social-evaluation-A precision (mean evaluations-list-A) 4   ;[-1,1]
  set non-social-evaluation-B precision (mean evaluations-list-B ) 4
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;reporters ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to-report random-normal-trunc [mid dev mmin mmax]
  ; creating a trunc normal function to be used for tuncating the normal distribution between mmin and mmax values
  let result random-normal mid dev
  while [result < mmin or result > mmax]
    [set result random-normal mid dev]
  report result
end

to-report normalized-min-max [norm-variable min-old max-old min-new max-new]
  let norm min-new + (((norm-variable - min-old) * (max-new - min-new)) / (max-old - min-old))
  report precision norm 4
end

to-report satisfying-status-BA [evaluations-list-BA]
 let satisfying-list-BA filter [i -> i > 0] evaluations-list-BA
 let satisfying-stat-BA abs sum satisfying-list-BA
 report satisfying-stat-BA
end

to-report dissatisfying-status-BA [evaluations-list-BA]
 let dissatisfying-list-BA filter [i -> i < 0] evaluations-list-BA
 let dissatisfying-stat-BA abs sum dissatisfying-list-BA
 report dissatisfying-stat-BA
end

to-report dissonance-status-A [sat-A dis-A] ; sat-A is satisfying-A and dis-A is dissatisfying-A
  let dissonant-A min (list sat-A dis-A)
  let consonant-A max (list sat-A dis-A)    ; in case of the same values, it does not matter
  let dissonance-of-A (2 * dissonant-A)/(dissonant-A + consonant-A)
  report dissonance-of-A
end

to-report dissonance-status-B [sat-B dis-B] ; sat-B is satisfying-B and dis-B is dissatisfying-B
  let dissonant-B min (list sat-B dis-B)
  let consonant-B max (list sat-B dis-B)    ; in case of the same values, it does not matter
  let dissonance-of-B (2 * dissonant-B)/(dissonant-B + consonant-B)
  report dissonance-of-B
end

to-report shading [dissonance-str]
 let shade normalized-min-max (-1 * dissonance-str) -1 0 -5 0
 report shade
end

to-report further-comparison-needed? [comparison-dimension-A comparison-dimension-B theoretical-range]
  let value 0
  ifelse abs (comparison-dimension-A - comparison-dimension-B) < 0.1 * theoretical-range  ;if the difference smaller than (0.1 * theoretical-range)
    [set value true]
    [set value false]
  report value
end


to-report relative-aspiration [aspiration-influencing aspiration-influenced] ; produces values <0 ; 1 > - weighing of the influenced agent's status
  ; for inquiring the influencing agent is the alter, who is influencing the ego
  ; for signlaing the influencing agent is the ego, who is influencing the alter
  let rel-aspiration 0.4 + aspiration-influencing - aspiration-influenced
  if 0.4 + aspiration-influencing - aspiration-influenced > 1 [set rel-aspiration 1]
  if 0.4 + aspiration-influencing - aspiration-influenced < 0 [set rel-aspiration 0]
  report precision rel-aspiration 3
end

to-report same-BA? [ego-val alter-val]
  ifelse ego-val = alter-val
  [report 1] ; 1 is the same BA
  [report 0] ; 0 is different BA
end

to-report sort-list [the-list prio-1 prio-2 prio-3] ; the same sorting reporter for inquiring and signaling lists, just used with different priority characteristics
  ; the-list = the nested list you want to sort ; prio-1 = the first prio characteristic; prio-2 = the second prio characteristic; prio-3 - the third prio characteristic
  let sorted sort-by [[?1 ?2] -> item prio-3 ?1 > item prio-3 ?2] the-list
  set sorted sort-by [[?1 ?2] -> item prio-2 ?1 > item prio-2 ?2] sorted
  set sorted sort-by [[?1 ?2] -> item prio-1 ?1 < item prio-1 ?2] sorted
  report sorted
end

to-report need-similarity [need-evaluation-ego need-evaluation-alter need-importance-ego need-importance-alter ]
  ;let evaluation-difference  need-evaluation-ego * need-evaluation-alter  ;;confirmation bias
  let evaluation-difference  abs (need-evaluation-ego - need-evaluation-alter)  ;;confirmation bias
  let importance-difference abs (need-importance-ego - need-importance-alter)

  ifelse evaluation-difference > 0
    [ report 0.4 * (1 - importance-difference)]  ;simplified
    [ report 0 ]
end

to-report new-need-satisfaction-inquiring [need-satisfaction-BA #item #item2]
  ; the #item refers to the number of item on the list, which designates inquiring persuasion for each need-satisfaction-BA
  ; the #item2 refers to the number of item on the list, which designates need-satisfaction-BA of alter
  ; done for experiential needs and values of both BAs
  let val (1 - item #item inquired-humat) * need-satisfaction-BA + item #item inquired-humat * item #item2 [self-attri-list] of ( humat item 0 inquired-humat)
  report val
end


; to do: whis is to take into account that signaled-humat is the convincing's humat list and here it assumes to be the convinced's humat list - to change
to-report new-need-satisfaction-signaling [need-satisfaction-BA #item]
  ; the #item refers to the number of item on the list, which designates signaling persuasion for each need-satisfaction-BA
  ; when humats are persuaded by other humats in their social networks, they change their satisfactions of needs for BAs to the extent that the alter is persuasive (status * similar of needs importances for the BA)
  ; reports a new value of needs satisfaction for a persuaded HUMAT
  ; done for experiential needs and values of both BAs
  report (1 - item #item signaling-humat) * need-satisfaction-BA + item #item signaling-humat * [need-satisfaction-BA] of myself
end

to-report ns [need-satisfaction-BA]
  let ns-alter [need-satisfaction-BA] of humat item 0 inquired-humat
  report ns-alter
end

to-report consistency             ;clustering, to gauge the consistency between the humat and their neighbors
  let act-behaviour 0
  let nr-same 0
  let nr-neighbors 0
  ask humats [
    set act-behaviour behaviour
    ask connection-neighbors [
      set nr-neighbors nr-neighbors + 1
      if behaviour = act-behaviour
      [set nr-same nr-same + 1]
    ]
  ]
ifelse nr-neighbors = 0
  [report 0]
  [report precision (nr-same / nr-neighbors) 2]
end

to eveluation
  set A-supporters  count humats with [behaviour = "A"]
  set B-supporters  count humats with [behaviour = "B"]
  set AB-supporters  count humats with [behaviour = "AB"]

  set A-value+ count humats with [behaviour = "A" and satisfaction-A > 0.02]
  set A-value- count humats with [behaviour = "A" and satisfaction-A < -0.02]
  set A-value count humats with [behaviour = "A" and satisfaction-A <= 0.02 and satisfaction-A >= -0.02]

  set B-value+ count humats with [behaviour = "B" and satisfaction-B > 0.02]
  set B-value- count humats with [behaviour = "B" and satisfaction-B < -0.02]
  set B-value count humats with [behaviour = "B" and satisfaction-B <= 0.02 and satisfaction-B >= -0.02]

  set AB-value+ count humats with [behaviour = "AB" and satisfaction / 10 > 0.02]
  set AB-value- count humats with [behaviour = "AB" and satisfaction / 10 < -0.02]
  set AB-value count humats with [behaviour = "AB" and satisfaction / 10 <= 0.02 and satisfaction / 10 >= -0.02]

  ifelse B-supporters > 0
    [set sr-supporters B-value+ / B-supporters]
    [set sr-supporters 0]

  ifelse A-supporters > 0
    [set sr-opponents A-value+ / A-supporters]
    [set sr-opponents 0]

 set satisfaction-rate (A-value+ + B-value+ + AB-value+) / count humats
 set support-rate B-supporters / count humats
end
@#$#@#$#@
GRAPHICS-WINDOW
348
19
959
631
-1
-1
3.0
1
12
1
1
1
0
0
0
1
0
200
0
200
1
1
1
ticks
60.0

BUTTON
23
555
273
588
SETUP-COMMUNITY
setup-community
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
24
596
145
629
GO ONCE
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
984
159
1103
204
% OPPONENTS
precision ((count humats with [behaviour = \"A\"] / count humats )* 100) 0
17
1
11

BUTTON
151
596
273
629
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
19
141
140
174
num-groups
num-groups
0
10
2.0
1
1
NIL
HORIZONTAL

MONITOR
1114
159
1234
204
% SUPPORTERS
precision ((count humats with [behaviour = \"B\"] / count humats )* 100) 0
17
1
11

PLOT
984
208
1365
328
SUPPORTER-or-OPPONENT?
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Opponents" 1.0 0 -5825686 true "" "plot A-supporters"
"Supporters" 1.0 0 -10899396 true "" "plot B-supporters"
"Neutrals" 1.0 0 -7500403 true "" "plot AB-supporters"

TEXTBOX
853
635
935
653
Support
13
0.0
1

TEXTBOX
291
571
348
591
Unhappy
13
0.0
1

TEXTBOX
303
53
344
71
Happy
13
0.0
1

TEXTBOX
611
637
697
656
Neutral
13
0.0
1

TEXTBOX
404
634
499
652
Oppose
13
0.0
1

TEXTBOX
20
115
260
133
STEP 2: Setting the community
14
14.0
1

TEXTBOX
19
217
238
236
STEP 3: Setting the group
14
14.0
1

OUTPUT
1118
37
1369
118
14

TEXTBOX
1181
10
1320
28
Group Information
14
14.0
1

BUTTON
85
394
212
427
ADD-AGENTS
ADD-AGENTS
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
18
276
269
309
less-or-more-vocal
less-or-more-vocal
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
18
238
269
271
against-or-for-change
against-or-for-change
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
18
351
270
384
num-agents
num-agents
0
100
5.0
5
1
NIL
HORIZONTAL

BUTTON
146
60
267
93
RESET
reset
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
19
69
130
87
STEP 1: Reset
14
14.0
1

TEXTBOX
25
534
266
552
STEP 4: Running the model
14
14.0
1

PLOT
984
334
1366
513
HAPPY(+), NEUTRAL() or UNHAPPY(-)?
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Opponents+" 1.0 0 -5825686 true "" "plot A-value+"
"Opponents-" 1.0 0 -2064490 true "" "plot A-value-"
"Opponents" 1.0 0 -534828 true "" "plot A-value"
"Supporters+" 1.0 0 -10899396 true "" "plot B-value+"
"Supporters-" 1.0 0 -13840069 true "" "plot B-value-"
"Supporters" 1.0 0 -2754856 true "" "plot B-value"
"Neutral+" 1.0 0 -12895429 true "" "plot AB-value+"
"Neutral-" 1.0 0 -7500403 true "" "plot AB-value-"
"Neutral" 1.0 0 -3026479 true "" "plot AB-value"

TEXTBOX
298
317
346
335
Neutral
13
0.0
1

SLIDER
143
141
270
174
time-voting
time-voting
50
120
100.0
10
1
NIL
HORIZONTAL

SWITCH
981
40
1104
73
show-links?
show-links?
0
1
-1000

SWITCH
982
77
1105
110
show-labels?
show-labels?
1
1
-1000

TEXTBOX
19
20
264
44
NOTE:  Follow the four steps.
16
14.0
1

TEXTBOX
983
17
1133
35
MODEL FEATURE:
14
14.0
1

TEXTBOX
986
137
1136
155
SIMULATION RESULT:
14
14.0
1

SLIDER
18
313
269
346
self-assertiveness
self-assertiveness
0
1
1.0
0.1
1
NIL
HORIZONTAL

PLOT
984
520
1366
644
OPINION-DISTRIBUTION
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.02 0 -16777216 true "" "histogram [ind-opinion] of humats"

SWITCH
81
440
219
473
intervention?
intervention?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?
This model aims to replicate the evolution of opinions and behaviours on a communal plan over time. It also aims to foster community dialogue on simulation outcomes, promoting inclusivity and engagement. Individuals (referred to as  **"agents"**), grouped based on Sinus Milieus (Groh-Samberg et al., 2023), face a binary choice: support or oppose the plan. Motivated by experiential, social, and value needs (Antosz et al., 2019), their decision is influenced by how well the plan aligns with these fundamental needs.

## HOW IT WORKS

During the model's operation, agents decide whether to support a plan, designated as the **"green hat"**, or oppose it, denoted as the **"red hat"**. Their choice between supporting or opposing is determined by a calculation that takes into account both their satisfaction with these options and the influence of social norms. Agents' satisfaction can be categorised into three dimensions based on their needs: experiential, social, and value-driven.

* Experiential need refers to your daily experience. Does the plan make your life more comfortable, and does it save money for example?

* Social need refers to your (dis)agreement with others. Do most of the people you know share your position, or are you the outlier in the group, making you an outcast?

* Value-driven need refers to important beliefs you have. For example, you may be very concerned about the environment, or you may be concerned about economic welfare.

Individual opinions remain dynamic through interactions with connected neighbours until they vote on the plan. During interactions, individuals may either convince others or may be convinced by others, and they may switch between being in favour or against the plan.


## HOW TO USE IT
To use this model, follow a four-step process. The initial three steps involve setting up the model: reset, setting the community, and setting the groups. Then, proceed to execute the model. Additional details are explained below.

### STEPS FOR IMPLEMENTATION AND ASSOCIATED COMMANDS AND PARAMETRES
To the left of the interface:

**Step 1: Reset the model**

The "RESET" completely clears all elements within the model's environment.

**Step 2: Setting the community**

During this step, establish the community based on specified characteristics. 

* **"num-groups"** determines the number of distinct groups within the simulated community, with a maximum value of 10 and a default setting of 2.

* **"time-voting"** specifies the duration for voting, setting the timeframe for model execution with a default value of 80.


**Step 3: Setting the group**

Groups have diverse properties, and agents within them vary. The model captures this diversity using two sliders for **"against-or-for-change"** and **"less-or-more-vocal"** characteristics to identify agents within groups.

* **"against-or-for-change"** reflects an agent's tendency to either maintain or change their behaviour, with values near 1 indicating openness to change and values near 0 signifying a preference for the status quo.

* **"less-or-more-vocal"** indicates an individual's vocal presence, where higher values suggest increased influence, with a value near 1 indicating strong vocal impact, while a value near 0 implies diminished vocal influence.

* **"num-agents"** allows the specification of the number of agents within a given group.

* **"ADD-AGENTS"** command enables the creation of agent groups by utilising selected parameters for "against-or-for-change," "less-or-more-vocal," and "num-agents." This combination allows the formation of diverse groups, such as a small outspoken faction in favor of a plan or a "silent majority" opposing a plan.

To form the extra group, adjust the three sliders as needed, then press the "ADD-AGENTS" button.

**Step 4: Running the model**

* **"SETUP-COMMUNITY"** command is employed to configure the entire system.

* **"GO-ONCE"** enables the model to execute for a single time step.

* **"GO"** function triggers ongoing execution of the model, enabling the generation of progress and outcomes based on the specified configurations.


### SYSTEM OUTPUT OVERVIEW
To the right of the interface:
#### MODEL FEATURE
* If the **"show-links?"** switch is on, the links between the agents become visible, showing the community structure; when the switch is off, links remain hidden.

* If **"show-labels?"** switch is on, the group ID becomes visible; when the switch is off, the group ID remains hidden.

* **"Group Information"** features an output box presenting detailed information specific to each group.

#### SIMULATION RESULTS

* The **"%OPPONENTS"** reveals the percentage of agents opposing a plan, while "%SUPPORTERS" indicates supporting agents, both evolving. 

* The **"SUPPORTERS-or-OPPONENT?"** graph depicts the temporal evolution of agent preferences between supporters (green hat) and opponents (red hat) within a specified timeframe.

* The **"HAPPY(+), NEUTRAL() or UNHAPPY(-)?"** chart gauges agent happiness based on their stance on the plan. Labels like "opponent+" signify happy opposing agents, "opponent-" indicates unhappy opposers, and "opponent" represents agents who oppose without a specific emotional state. Similarly, "supporters+", "supporters-", and "supporters" denote happiness, unhappiness, or neutrality among supporting agents.

## THINGS TO NOTICE

Emphasising the importance of maintaining static parameters during model execution, it is crucial to note that any changes to the remaining parameters should be avoided. 

Moreover, as the model operates, the connections among individuals become clearer, indicating a higher frequency of interactions. Conversely, weakened connections suggest a decrease in interaction frequency, while vanished connections signify prolonged absence of interaction.


## NETLOGO FEATURES

In this conceptual framework, the organisation of visual elements follows a designated symbolic structure:

* Individual agents are symbolised by **nodes or heads**.

* The relationships between these agents are represented by **connecting lines or edges**. "Dashed lines" signify connections between different groups, while "solid lines" depict connections within the same group.

* Distinct groups are identified by the **colour of each agent's face**.

* The specific opinions of agents are indicated by the **colour of their hats**, denoted as either "green" or "red."

* The importance of agents' vocal contributions is reflected in the **size of their representations**.

* **Facial expressions**, depicted as happy, sad, or neutral faces, convey the satisfaction, dissatisfaction, or indifference experienced by these agents.


## CREDITS AND REFERENCES

* Groh-Samberg O, Schröder T, Speer A. Social milieus and social integration. From theoretical considerations to an empirical model. KZfSS KZfSS Cologne Journal for Sociology and Social Psychology. 2023 Jul 5:1-25.

* Antosz P, Jager W, Polhill G, Salt D, Alonso-Betanzos A, Sánchez-Maroño N, Guijarro-Berdiñas B, Rodríguez A. Simulation model implementing different relevant layers of social innovation, human choice behaviour and habitual structures. SMARTEES Deliverable. 2019.
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
Circle -16777216 true false 30 30 240

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

neutral 1
false
0
Circle -2064490 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 10
false
0
Circle -1184463 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 2
false
0
Circle -13791810 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 3
false
0
Circle -955883 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 4
false
0
Circle -11221820 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 5
false
0
Circle -14835848 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 6
false
0
Circle -13345367 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 7
false
0
Circle -8630108 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 8
false
0
Circle -13840069 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

neutral 9
false
0
Circle -6459832 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Rectangle -16777216 true false 105 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

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

sad 1
false
0
Circle -2064490 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 10
false
0
Circle -1184463 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 2
false
0
Circle -13791810 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 3
false
0
Circle -955883 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 4
false
0
Circle -11221820 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 5
false
0
Circle -14835848 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 6
false
0
Circle -13345367 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 7
false
0
Circle -8630108 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 8
false
0
Circle -13840069 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

sad 9
false
0
Circle -6459832 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 210 105 225 90 240 90 240 90 255 90 262 120 240 150 225 192 247 210 262 210 255 210 240 210 240 195 225
Polygon -7500403 true true 150 15 45 120 255 120

smile 1
false
0
Circle -2064490 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 10
false
0
Circle -1184463 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 2
false
0
Circle -13791810 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 3
false
0
Circle -955883 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 4
false
0
Circle -11221820 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 5
false
0
Circle -14835848 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 6
false
0
Circle -13345367 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 7
false
0
Circle -8630108 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 8
false
0
Circle -13840069 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

smile 9
false
0
Circle -6459832 true false 60 105 180
Circle -16777216 true false 105 150 30
Circle -16777216 true false 165 150 30
Polygon -16777216 true false 150 255 105 240 90 225 90 225 90 210 90 203 120 225 150 240 192 218 210 203 210 210 210 225 210 225 195 240
Polygon -7500403 true true 150 15 45 120 255 120

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
set layout? false
set plot? false
setup repeat 300 [ go ]
repeat 100 [ layout ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment(intervention)" repetitions="1000" runMetricsEveryStep="true">
    <setup>reset
setup-community</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>A-supporters</metric>
    <metric>B-supporters</metric>
    <metric>AB-supporters</metric>
    <metric>A-value+</metric>
    <metric>A-value-</metric>
    <metric>A-value</metric>
    <metric>B-value+</metric>
    <metric>B-value-</metric>
    <metric>B-value</metric>
    <metric>AB-value+</metric>
    <metric>AB-value-</metric>
    <metric>AB-value</metric>
    <metric>satisfaction-rate</metric>
    <steppedValueSet variable="Thre_asse" first="0.1" step="0.1" last="1"/>
    <steppedValueSet variable="Thre_conf" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="intervention?">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment(scenario2and3)" repetitions="1" runMetricsEveryStep="true">
    <setup>reset
setup-community</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>A-supporters</metric>
    <metric>B-supporters</metric>
    <metric>AB-supporters</metric>
    <metric>A-value+</metric>
    <metric>A-value-</metric>
    <metric>A-value</metric>
    <metric>B-value+</metric>
    <metric>B-value-</metric>
    <metric>B-value</metric>
    <metric>AB-value+</metric>
    <metric>AB-value-</metric>
    <metric>AB-value</metric>
    <metric>sr-supporters</metric>
    <metric>sr-opponents</metric>
    <metric>satisfaction-rate</metric>
    <metric>support-rate</metric>
    <enumeratedValueSet variable="Thre_asse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Thre_conf">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intervention?">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment(intervention) (sensitivity)" repetitions="1" runMetricsEveryStep="true">
    <setup>reset
setup-community</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>A-supporters</metric>
    <metric>B-supporters</metric>
    <metric>AB-supporters</metric>
    <metric>A-value+</metric>
    <metric>A-value-</metric>
    <metric>A-value</metric>
    <metric>B-value+</metric>
    <metric>B-value-</metric>
    <metric>B-value</metric>
    <metric>AB-value+</metric>
    <metric>AB-value-</metric>
    <metric>AB-value</metric>
    <metric>sr-supporters</metric>
    <metric>sr-opponents</metric>
    <metric>satisfaction-rate</metric>
    <metric>support-rate</metric>
    <steppedValueSet variable="Thre_asse" first="0.2" step="0.2" last="1"/>
    <steppedValueSet variable="Thre_conf" first="0.2" step="0.2" last="1"/>
    <enumeratedValueSet variable="intervention?">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment(scenario1)" repetitions="1" runMetricsEveryStep="true">
    <setup>reset
setup-community</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>A-supporters</metric>
    <metric>B-supporters</metric>
    <metric>AB-supporters</metric>
    <metric>A-value+</metric>
    <metric>A-value-</metric>
    <metric>A-value</metric>
    <metric>B-value+</metric>
    <metric>B-value-</metric>
    <metric>B-value</metric>
    <metric>AB-value+</metric>
    <metric>AB-value-</metric>
    <metric>AB-value</metric>
    <metric>sr-supporters</metric>
    <metric>sr-opponents</metric>
    <metric>satisfaction-rate</metric>
    <metric>support-rate</metric>
    <enumeratedValueSet variable="Thre_asse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Thre_conf">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intervention?">
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

inter-groups
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
