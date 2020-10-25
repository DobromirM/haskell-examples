cup ml = \message -> message ml

getMl aCup = aCup (\ml -> ml)

drink aCup amount = cup (ml - amount)
  where ml = getMl aCup

isEmpty aCup = getMl aCup == 0

--------------------------------------------

robot (name, attack, hp) = \message -> message (name, attack, hp)
name (name, _, _) = name
attack (_, attack, _) = attack
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot(n, newAttack, h))
setHp aRobot newHp = aRobot (\(n, a, _) -> robot(n, a, newHp))

printRobot aRobot = aRobot (\(n, a, h) -> n ++
                                          " attack:" ++ (show a) ++
                                          " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) ->
                                      robot (n, a, h - attackDamage))

fight attacker defender = damage defender attack
  where attack = if getHp attacker > 10
                 then getAttack attacker
                 else 0

killerRobot = robot ("Kill3r", 25, 200)
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHp killerRobot 50
gentleGiant = robot ("Mr. Friendly", 10, 300)

--gentleGiantRound1 = fight killerRobot gentleGiant
--killerRobotRound1 = fight gentleGiant killerRobot
--gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
--killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
--gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
--killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
