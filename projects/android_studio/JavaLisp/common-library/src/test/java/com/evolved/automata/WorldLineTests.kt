package com.evolved.automata

import com.evolved.automata.nn.util.TallyVector
import com.evolved.automata.nn.util.UnsignedNumVectorType
import org.hamcrest.CoreMatchers.*
import org.junit.Assert.assertThat
import org.junit.Assert.assertTrue
import org.junit.Test
import kotlin.random.Random


class WorldLineTests {



    @Test
    fun testCheckIfAValueExists(){
        val worldLine = WorldLine()

        val desc = "x"

        assertTrue("Failed to not detecte value: ", !worldLine.hasValue(desc))
    }

    @Test
    fun testStoreRetrieveMostRecentValue() {
        val worldLine = WorldLine()

        val desc = "x"

        val value = SetCogject(arrayOf("x", "y", "x+y", "y+x", "x>y", "x is positive"), setOf("x", "x is positive"))

        var message = "Failed to save value"
        val time = 0L
        try
        {
            worldLine.setValue(desc, value, time)

            // get last saved value

            val retrieved = worldLine.getLastValue(desc, time)

            message = "Failed to retrieve a value"
            assertTrue(message, retrieved!=null)

            if (retrieved!=null){
                message = "Retrieved wrong value, expected $value but found $retrieved"
                assertTrue(message, value.toString().equals(retrieved.toString()))
            }

            // Store multiple values
            val nextValue = IntCogject(5, 10)
            worldLine.setValue(desc, nextValue, time + 5)

            message = "Failed to retrieve first value"
            val searchTime = time + 3L
            val firstRetrieved = worldLine.getLastValue(desc, searchTime)

            assertTrue(message, firstRetrieved != null)
            if (firstRetrieved != null) {
                message = "Expected $value but found $firstRetrieved"
                assertTrue(message, value.toString().equals(firstRetrieved.toString()))
            }

            message = "Expected to retrieve second value on exact match"
            val nextRetrieved = worldLine.getLastValue(desc, time + 5)

            assertTrue(message, nextValue.toString().equals(nextRetrieved.toString()))



            val lastRetrieved = worldLine.getLastValue(desc, time + 20)
            message = "Expected $nextValue but found $lastRetrieved"
            assertTrue(message, nextValue.toString().equals(lastRetrieved.toString()))


            val inserted = SetCogject(arrayOf("x", "y", "x+y", "y+x", "x>y", "x is positive"), setOf("x", "x is positive", "x+y", "x>y"))
            worldLine.setValue(desc, inserted, time + 2)

            val retrieveInserted = worldLine.getLastValue(desc, time + 3)
            message = "Failed to retrieve correct [$desc] version.  Expected $inserted but found $retrieveInserted"

            assertTrue(message, retrieveInserted.toString().equals(inserted.toString()))
        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    @Test fun testRemoveLatestValue(){
        var message = "Failed to create WorldLine"
        try
        {
            val timeLine = WorldLine()
            val desc = "x"

            val firstValue = SetCogject(arrayOf("x", "y", "x+y", "y+x", "x>y", "x is positive"), setOf("x", "x is positive"))
            message = "Failed to add cogject: $firstValue"
            timeLine.setValue(desc, firstValue, 0L)

            val secondValue = IntCogject(10, 15)
            message = "Failed to add cogject: $secondValue"
            timeLine.setValue(desc, secondValue, 5L)

            message = "Failed to delete first value"

            timeLine.removeLatestValue(desc, 2)

            assertTrue(message, !timeLine.hasValue(desc, 3))

            assertTrue(message, timeLine.getLastValue(desc, 3) == null)
        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    @Test fun testRemoveKey(){
        var message = "Failed to create WorldLine"
        try
        {
            val timeLine = WorldLine()
            val desc = "x"

            val firstValue = SetCogject(arrayOf("x", "y", "x+y", "y+x", "x>y", "x is positive"), setOf("x", "x is positive"))
            message = "Failed to add cogject: $firstValue"
            timeLine.setValue(desc, firstValue, 5L)

            message = "Failed to find key after creation"

            assertTrue(message, timeLine.hasValue(desc, 5))
            assertTrue(message, timeLine.hasValue(desc, 15))

            message = "Failed to not find key before creation"

            assertTrue(message, !timeLine.hasValue(desc, 3))

            val success = timeLine.removeKey(desc, 6L)
            message = "Failed to not remove key after value"
            assertTrue(message, success)

            message = "Failed to verify continued existence of old key"
            assertTrue(message, timeLine.hasValue(desc, 5))

            assertTrue(message, !timeLine.hasValue(desc, 15))

        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun testGetState(){

        var message = "Failed to create timeline"
        try
        {
            val timeLine = WorldLine()
            val desc = "x"

            message = "Failed to create value"
            val firstValue = IntCogject(1, 1)

            message = "Failed to set item"
            timeLine.setValue("start", firstValue, -1)

            timeLine.setValue("x", firstValue, 0)

            timeLine.setValue("x is positive", firstValue, 0)
            timeLine.setValue("y", firstValue, 0)
            timeLine.setValue("y is positive", firstValue, 0)
            timeLine.setValue("x>y", firstValue, 0)
            timeLine.setValue("t>x", firstValue, 0)
            timeLine.setValue("t", firstValue, 0)

            timeLine.setValue("t>y", firstValue, 5)
            timeLine.setValue("y<t", firstValue, 5)

            timeLine.setValue("t is positive", firstValue, 10)

            var result = timeLine.getState(-1)

            assertTrue(message, result.containsKey("start") && result.size == 1)

            message = "Failed to get updated state after removal of value"
            result = timeLine.getState(0)
            println("Result: $result")
            assertTrue(message, result != null && result.size == 8)

            timeLine.removeLatestValue("start", -1)

            result = timeLine.getState(0)

            println("Result after removable: $result")
            assertTrue(message, result != null && result.size == 7)
        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    @Test fun testGetTemporalInfoSet(){
        var message = "Failed to "
        try
        {
            val timeLine = WorldLine()
            val desc = "x"

            message = "Failed to create value"

            val base = 100L
            val offset = 50
            val timeSet: MutableSet<Long> = mutableSetOf()

            for ((i, key) in arrayOf("x", "y", "z", "x", "x", "y").withIndex()) {
                val time = base + i*offset
                timeSet.add(time)
                message = "Failed to create cogject with $i at time ${i*offset + base}"
                timeLine.setValue(key, IntCogject(i , 10) ,time )
                assertTrue(message, timeLine.hasValue(key, time))
            }

            val temporalInstances = timeLine.getInstancesBetween(0, 400)

            println("Temporal instances: $temporalInstances")

            message = "Failed to retrieve all encountered instances"
            assertTrue(message, temporalInstances.size > 0)

            temporalInstances.forEach{instance ->
                message = "Failed to find $instance in $timeSet"
                assertTrue(message, timeSet.contains(instance))
            }


        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun testProcessingDecay() {
        var message = "Failed to "
        try
        {
            val timeLine = WorldLine()
            val controlKey = "count"
            val updatedKey = "out"

            message = "Failed to create decaying cogject"

            val threshold = 150L
            var timeoutCogject = object: IntCogject(0, 1) {
                override fun process(world: WorldLine, processTime: Long): FloatArray {

                    if (this.name != null) {
                        val myName = this.name?:""
                        val lastUpdateTime = world.getLastEntry(myName, processTime)
                        if (lastUpdateTime != null) {
                            if (threshold < (processTime - lastUpdateTime.updateTime)){
                                world.removeKey(myName, processTime)
                            }
                            stateValue[0] = getBooleanIntValue(threshold >= (processTime - lastUpdateTime.updateTime))
                        }

                    }

                    return super.process(world, processTime)
                }
            }

            message = "Failed to add decaying cogject"
            timeoutCogject.name = controlKey
            timeLine.setValue(controlKey, timeoutCogject, 0)

            for (time in longArrayOf(0, 10, 20, 30, 40, 50, 100, 200)){
                timeLine.process(time)
                val exists = timeLine.hasValue(controlKey)
                message = "Failed to process cogject at time $time decay.  Decay expected: ${(time >= threshold)} decay actual: ${!exists}"
                assertTrue(message, exists == time < threshold)
            }


        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun testSpecialTimeFunctions(){
        var message = "failed to add cogject"

        try {
            var key = "x"
            val timeLine = WorldLine()
            var response = timeLine.getGroupedUpdateInstances(key)

            message = "Failed to receive empty list"
            assertTrue(message, response.size == 0)

            val expectedTimes = longArrayOf(0, 10, 20, 30, 40, 50, 60)

            for ((index, time) in expectedTimes.withIndex()){
                val cogject = IntCogject(index*2, 100)

                message = "Failed to create $cogject at time $time"
                timeLine.setValue(key, cogject, time)
                println("Create new object: $cogject at time $time")
                message = "Failed to receive group history response for [$key]"
                response = timeLine.getGroupedUpdateInstances(key)
                println("Found response: " + response)
                assertTrue(message, response.size == 1 && response[0].size == (index + 1) && expectedTimes.withIndex().all {pair -> val (i, updateTime) = pair; (i <= index) && response[0].contains(updateTime) || i > index})
            }

            val multiSegmentTimes = longArrayOf(0, 10, 20, 30, 40, 50, 60, -1, 70, 80, 90, 100)

            println("Testing partition of cogject history with removal")
            key = "y"
            var prevLength = 0
            var responseLength = 0
            for ((index, time) in multiSegmentTimes.withIndex()){
                if (time < 0) {
                    timeLine.removeKey(key, (multiSegmentTimes[index-1] + multiSegmentTimes[index+1])/2)
                    println("Deleting key $key at time $time")
                    message = "Failed to get new "
                    prevLength = responseLength
                }
                else {

                    val cogject = IntCogject(index*2, 100)

                    message = "Failed to create $cogject at time $time"
                    timeLine.setValue(key, cogject, time)
                    println("Created new object: $cogject at time $time")
                    message = "Failed to receive new response"
                    response = timeLine.getGroupedUpdateInstances(key)

                    responseLength = response.size

                    println("Received response: $response")
                    message = "Failed to extend length of responses.  Expected $prevLength == ${responseLength - 1}"
                    assertTrue(message, prevLength<0 || prevLength == responseLength - 1)
                    prevLength = -1
                }
            }


            key = "y"
            message = "Failed to get next event time for \"$key\""
            var expectedNextTime:Long? = 0
            var nextEventTime = timeLine.findNextValueTime("y", -1)
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"

            assertTrue(message, expectedNextTime == nextEventTime)

            nextEventTime = timeLine.findNextValueTime("y", 50)
            expectedNextTime = 50
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)

            nextEventTime = timeLine.findNextValueTime("y", 51)
            expectedNextTime = 60
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)

            nextEventTime = timeLine.findNextValueTime("y", 61)
            expectedNextTime = 70
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)

            message = "Failed to find next event value when not ignoring boundary for $key at time 61"
            nextEventTime = timeLine.findNextValueTime("y", 61, ignoreRemovalBoundary = false)
            expectedNextTime = null
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)

            key = "x"
            message = "Failed to not find next value from end of key \"x\""
            nextEventTime = timeLine.findNextValueTime(key, 61, ignoreRemovalBoundary = false)
            expectedNextTime = null
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)

            key = "x"
            message = "Failed to not find next value from end of key \"$key\""
            nextEventTime = timeLine.findNextValueTime(key, 61, ignoreRemovalBoundary = true)
            expectedNextTime = null
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)


            key = "y"
            message = "Failed to not find next value from end of key \"$key\""
            nextEventTime = timeLine.findNextValueTime(key, 101, ignoreRemovalBoundary = false)
            expectedNextTime = null
            message = "Failed to find correct next event time for $key.  Expected $expectedNextTime but received $nextEventTime"
            assertTrue(message, expectedNextTime == nextEventTime)
        }
        catch (t: Throwable){
            t.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun testCausalProcessing(){
        var message = ""
        try {
            var initialKey = "x"
            val timeLine = WorldLine()

            println("Testing adding decay to an existing Cogject")

            var baseCogject = IntCogject(10, 20).apply{name = initialKey}


            fun addDecay(cogject: Cogject, lifespan: Long): Cogject {

                return cogject.apply { processInterceptor =
                        { world: WorldLine, processTime: Long ->
                            val cogjectName = cogject.name?:""
                            val lastUpdateTime = world.getLastEntry(cogjectName, processTime)?.updateTime?:processTime
                            if ((processTime - lastUpdateTime) >= lifespan){
                                world.removeKey(cogjectName, processTime)
                            }
                            stateValue
                        } }
            }

            message = "Failed to add decay capability to cogject $baseCogject"
            val decayLife = 20L
            addDecay(baseCogject, decayLife)
            val addTime = 10L
            for (time:Long in 1L..100L) {

                if (time == addTime) {

                    timeLine.setValue(initialKey, baseCogject, time)
                }
                timeLine.process(time)
                val keyExists = timeLine.hasValue(initialKey, time)
                message = "Failed to perform appropriate action after decay time: $time.  Object exists: $keyExists"

                assertTrue(message, time < addTime + decayLife && keyExists || !keyExists)
            }


            println("Testing creating cogject sequence")

            fun WorldLine.getLastUpdateTime(cogject: Cogject, time: Long): Long? {

                val entry = getLastEntry(cogject?.name?:"", time)
                return entry?.updateTime
            }

            fun WorldLine.removeMe(me: Cogject, time:Long): WorldLine {
                removeKey(me.name?:"", time)
                return this
            }

            fun WorldLine.addCogject(cogject: Cogject, time: Long){
                val name = cogject.name
                if (name != null) {
                    setValue(name, cogject, time)
                }
            }

            fun createCogjectDecaySequence(period:Long, cogjectList: List<Cogject>): Cogject? {
                if (cogjectList.size == null) {
                    return null
                }
                else
                    return cogjectList.first().interceptor{world: WorldLine, processTime: Long ->
                        val createTime = world.getLastUpdateTime(this, processTime)?:processTime
                        if (processTime - createTime > period) {
                            world.removeMe(this, processTime)
                            if (cogjectList.size > 1) {
                                val nextCog = createCogjectDecaySequence(period, cogjectList.drop(1))
                                if (nextCog != null)
                                    world.addCogject(nextCog, processTime + 1)
                            }
                        }
                        stateValue
                    }
            }

            val firstCogject = createCogjectDecaySequence(10, listOf(
                    IntCogject(1, 100).apply { name = "a"},
                    IntCogject(1, 100).apply { name = "b"},
                    IntCogject(1, 100).apply { name = "c"},
                    IntCogject(10, 100).apply { name = "x"},
                    IntCogject(10, 100).apply { name = "y"},
                    IntCogject(10, 100).apply { name = "z"}))

            if (firstCogject != null)
                timeLine.addCogject(firstCogject, 0)

            for (time in 0L..100) {
                val initialState = timeLine.getState(time)
                timeLine.process(time)
                val finalState = timeLine.getState(time)
                println("$time) timeline transition: $initialState -> $finalState")
            }

            println("Testing state machine cogjects")

            infix fun String.yields( action: StateMachineCogject.(WorldLine,Long)->Unit) : Pair<String, (StateMachineCogject.(WorldLine,Long)->Unit)> {
                return Pair(this, action)
            }

            var stateMachine =  StateMachineCogject("machine",
                    "initial" `yields`   {world:WorldLine, time:Long -> if (time - lastStateTransition > 10) setNextState("speak", time);  stateValue},
                    arrayOf(
                    "speak" `yields` {world:WorldLine, time:Long ->
                        if (time - lastStateTransition == 1L)
                            println("Hello, World")
                        if (time - lastStateTransition > 10)
                            setNextState("reply", time)

                        stateValue},
                    "reply" `yields` {world:WorldLine, time:Long ->
                        if (time - lastStateTransition == 1L)
                            println("what do you want?")
                        if (time - lastStateTransition > 10)
                            world.removeMe(this, time)
                        stateValue}))


            timeLine.setValue(stateMachine, 0)
            for (time in 1L..100) {
                println("State: ${timeLine.getState(time)}")
                timeLine.process(time)
            }
        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun goalSearchTests(){
        var message = "Failed to add basic cogject"

        try {
            // Test creating relative goals
            val userName = "user"
            println("Test creating effective goals")
            var world = WorldLine(userCogjectName = userName)

            val name = "x"
            world.setValue(name, IntCogject(5, 10), 10)

            message = "Failed to create simple name goal"

            val simpleNameGoal = Goal(cogjectWithName = name)

            println("Testing goal fulfillment")

            message = "Goal should not be fullfilled when time less than 10"
            assertTrue(message, !world.goalIsSatisfied(simpleNameGoal, 0))

            message = "Goal should  be fullfilled when time is 10"
            assertTrue(message, world.goalIsSatisfied(simpleNameGoal, 10))

            message = "Goal should be fullfilled when time is greater than 10"

            assertTrue(message, world.goalIsSatisfied(simpleNameGoal, 100))

            println("Test simple goal search where goal is already reached")

            world.setValue("user", IntCogject(1,1), 0)

            val actionsToTake: List<PlanningStage>?

            actionsToTake = world.planRouteToGoal(10, simpleNameGoal)
            assertTrue(message, actionsToTake!=null && actionsToTake.isEmpty())

            // Interlude for testing capabilities
            testSimpleNameCapabilities(name);



        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    fun testSimpleNameCapabilities(name: String){

        val simpleGoal = Goal(cogjectWithName = name)

        var message = "Failed to create simple name goal"

        var world = WorldLine()

        val name = simpleGoal.cogjectWithName!!

        println("----> Testing capabilities")

        message = "Failed to create ability to add random int cogject"
        // This capability is only able to create goals that have not specific value of time
        // requirements
        val abilityToCreateRandomCogject = object: Capability(goalCanAchieve = Goal(cogjectWithName = name), confidence = object: Confidence {
            override fun getConfidence(goal: Goal?): Float {
                return 1F
            }
        }){
            override fun doIt(goalToAchieve: Goal,requestTime: Long, currentTime: Long, timeLine: WorldLine) {
                timeLine.setValue(goalToAchieve.cogjectWithName!!, IntCogject(Random.nextInt(0, 10), 10, goalToAchieve.cogjectWithName!!), currentTime)
            }
        }

        message = "Failed to obtain a favor"

        var favor = abilityToCreateRandomCogject.requestFavor(world, simpleGoal, 0)

        assertTrue(message, favor != null)

        message = "Failed to correctly process favor of world: ${world.getState(0)} and time 0"

        val status = favor?.process(world, 0)

        message = "Failed to complete favor with correct status: $status"

        assertTrue(message, status == FAVOR_STATUS.SUCCESS)

        message = "Failed to correctly call world.goalIsSatisfied(simpleGoal, 0)"
        var goalSatisfied = world.goalIsSatisfied(simpleGoal, 0)


        message = "Failed to verify goal is satisfied in the world: isSatisfied = $goalSatisfied"

        assertTrue(message,  goalSatisfied)

        println("worldline: ${world.getState(1)}")

        println("Testing goal fullfillment at a particular time")

        val delayedSimpleGoal = Goal(cogjectWithName = name, observeAtTime = 10)

        message = "Failed to remove all keys from world"
        world.removeAllValues(name)

        assertTrue(message, world.getLastValue(name, 0) == null)
        message = "Failed to obtain favor"

        favor = abilityToCreateRandomCogject.requestFavor(world, delayedSimpleGoal, 0)

        assertTrue(message, favor != null)

        favor?.process(world, 0)

        message = "Failed to detect goal satisfied after 10 steps"

        assertTrue(message, world.goalIsSatisfied(delayedSimpleGoal, 10))

        message = "Goal fails to be not satisfied with t < 10"

        assertTrue(message, !world.goalIsSatisfied(delayedSimpleGoal, 9))

        message = "Goal fails to be not satisfied with t > 10"

        assertTrue(message, !world.goalIsSatisfied(delayedSimpleGoal, 11))

        message = "Failed to satisfy goal after 10 steps"
        for (t: Long in 0L..20) {

            if (world.getLastValue(name, t)==null){
                val result = favor?.process(world, t)
                println("Favor status: $result")
            }

            println("Current world: ${world.getState(t)} at time $t")
            assertTrue(message, t != 10L && !world.goalIsSatisfied(delayedSimpleGoal, t) || t==10L && world.goalIsSatisfied(delayedSimpleGoal, t))
        }

        println("Test goal ranges")

        val pivotTime = 10L
        val simpleFutureGoal = Goal(cogjectWithName = name, observeAfter = pivotTime)

        goalSatisfied = world.goalIsSatisfied(simpleFutureGoal, 4)

        message = "Failed to not match $simpleFutureGoal when time less than $pivotTime.  Expected false but found $goalSatisfied"

        assertTrue(message, !goalSatisfied)

        favor = abilityToCreateRandomCogject.requestFavor(world, simpleFutureGoal, 0)

        for (t: Long in 0L..20) {
            println("Current world: ${world.getState(t)} at time $t")
            assertTrue(message, t < pivotTime && !world.goalIsSatisfied(simpleFutureGoal, t) || t>=pivotTime && world.goalIsSatisfied(simpleFutureGoal, t))
        }
    }


    @Test fun testRealGoalSearch(){

        var message = ""
        try {

            val userCogjectName = "USER"
            val ACogjectName = "A"
            val BCogjectName = "B"


            val abilityToCreateB = object: Capability(goalCanAchieve = Goal(cogjectWithName = "B")){
                override fun doIt(goalToAchieve: Goal, requestTime: Long, currentTime: Long, timeLine: WorldLine) {
                    val item = RandomIntCogject(goalCanAchieve.cogjectWithName!!)

                    timeLine.setValue(desc = goalCanAchieve.cogjectWithName!!, value = item, time = currentTime )
                }
            }

            val ACogject = IntCogject(0, 6, ACogjectName).addCapability(abilityToCreateB)

            val CCogjectName = "C"
            val abilityToCreateC = object: Capability(goalCanAchieve = Goal(cogjectWithName = CCogjectName)){
                override fun doIt(goalToAchieve: Goal, requestTime: Long, currentTime: Long, timeLine: WorldLine) {
                    val item = IntCogject(4,10, goalCanAchieve.cogjectWithName!!)

                    timeLine.setValue(desc = goalCanAchieve.cogjectWithName!!, value = item, time = currentTime )
                }
            }


            val BCogject = IntCogject(0, 6, BCogjectName).addCapability(abilityToCreateC)


            val abilityToCreateARandomCogject = object: Capability(goalCanAchieve = Goal()){
                override fun doIt(goalToAchieve: Goal, requestTime: Long, currentTime: Long, timeLine: WorldLine) {
                    val item = RandomIntCogject(goalToAchieve.cogjectWithName!!)

                    timeLine.setValue(desc = goalToAchieve.cogjectWithName!!, value = item, time = currentTime )
                }
            }

            val abilityToCreateAnyIntCogject = object: Capability(goalCanAchieve = Goal()){
                override fun doIt(goalToAchieve: Goal, requestTime: Long, currentTime: Long, timeLine: WorldLine) {

                    timeLine.setValue(desc = goalToAchieve.cogjectWithName!!, value = goalToAchieve.cogjectWithValue?:RandomIntCogject(UNKNOWN_ITEM_NAME), time = currentTime )
                }
            }

            val user = IntCogject(1, 10, userCogjectName)
            user.addCapability(abilityToCreateARandomCogject)
            user.addCapability(abilityToCreateAnyIntCogject)

            var world = WorldLine()

            world.setValue(user, 0)

            message = "Failed to obtain goal confidence"

            val gift = Goal("Money")

            val goalConfidence = user.getGoalConfidence(gift)

            assertTrue(message, goalConfidence == 1F)

            val rent = Goal("Rent", cogjectWithValue = IntCogject(5, 10))

            message = "Failed to verify that user can create any amount of rent money"
            assertTrue(message, user.getGoalConfidence(rent) > 0)

            message = "Failed to request favor of user object"



            val onFavorComplete = {success: FAVOR_STATUS -> println("Finished satisfying goal: $success")}
            var favorAccepted = user.requestFavor(gift, world, 0, onFavorComplete)
            message = "Failed to get favor acceptance status"
            assertTrue(message, favorAccepted)

            message = "Failed to process favor"

            world.process(0)
            world.process(1)

            message = "Failed to find a gift: ${world.getState(1)}"

            val hasGift = world.getLastEntry("Money", 1)
            assertTrue(message, hasGift!=null)

            message = "Failed to get committment to pay rent"

            favorAccepted = user.requestFavor(rent, world, 1+1, onFavorComplete)

            assertTrue(message, favorAccepted)

            world.process(2)
            world.process(3)

            val rentPaid = world.goalIsSatisfied(rent, 3)

            assertTrue(message, rentPaid)

            println("World: ${world.getState(3)}")

            world.setValue(ACogject, 0)

            // Model
            message = "creating transition models"
            val transitionCogjectName = "transition-examples"
            val baseTime = 10L
            world.setValue(IntCogject(0, 5, transitionCogjectName), baseTime)

            world.setValue(ACogject, baseTime)
            world.removeKey(userCogjectName, baseTime + 1)

            world.setValue(BCogject, baseTime+1)

            world.removeKey(ACogjectName, baseTime+2)

            world.setValue(IntCogject(0,5, CCogjectName), baseTime+ 10)

            world.removeKey(BCogjectName, baseTime+12)

            world.setValue(IntCogject(0, 1, transitionCogjectName), baseTime + 20)

            val goalCogject = Goal(cogjectWithName = CCogjectName)

            message = "Failed to find cogjects capable of achieving $goalCogject"
            var searchTemporalRadius = baseTime

            val possibleCauses = world.findCapabilities(goalCogject, baseTime+15, searchTemporalRadius)

            val expectedCause = BCogject

            assertTrue(message, !possibleCauses.isEmpty())

            message = "Failed to find only $BCogject as a cause"

            println("Possible causes: $possibleCauses")
            assertTrue(message, possibleCauses.size == 1)

            assertTrue(message, possibleCauses.any {cogTime -> cogTime.cogject == expectedCause})

            message = "Failed to obtain update times between ${baseTime} and ${baseTime+11}"
            val updateTimes = longArrayOf(0L,1,2, 10, 12,20).map{it + baseTime}

            val receivedTimes = world.getUpdateTimes(baseTime, baseTime+21)

            message = "Failed to match update times.  Expected $updateTimes but found $receivedTimes"

            assertTrue(message, updateTimes.size == receivedTimes.size && updateTimes.withIndex().all{(i, v)->v == receivedTimes[i]} )


            val executeTime = 40L
            val goalTime = executeTime + 10
            val expectedSolutionPlan = listOf(PlanningStage(ACogject, Goal(cogjectWithName = CCogjectName), executeTime))

            world.removeKey(BCogjectName, executeTime-1)
            world.setValue(ACogject, executeTime)
            message = "Failed to derive solution"
            val solutionPlan = world.planRouteToGoal(executeTime, ACogject, Goal(cogjectWithName = CCogjectName))

            assertTrue(message, solutionPlan!=null && solutionPlan.size>1)

            println("Solution: $solutionPlan")


        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test fun testIncrementFunctions(){

        var message = ""
        try {
            var map = mutableMapOf<String, Int>()

            println("");
        }
        catch (e: Exception){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    fun String.cog(): Cogject {
        return IntCogject(1, 1, this)
    }


    fun String.query(): Pair<Cogject, ((Cogject) ->Boolean)?> {
        return Pair(this.cog(), {cog: Cogject -> cog.name == String@this})
    }


    fun String.goal(): Goal {
        return Goal(cogjectWithName = this)
    }



    @Test fun testWorldLineUtils(){


        var message = "Failed to create UnsignedNumVectorType"

        try {
//            val binaryVectorType = object: VectorType(VectorType.FIELD_TYPE_NAME.UNSIGNED_BINARY_NUMBER, 32) {
//                override fun makeValidator(): LearningConfiguration.InputValidator {
//                    return
//                }
//
//            }


            val type = UnsignedNumVectorType(23)

            message = "Failed to convert 10 into a float array"

            val serialized = type.valueToVector(10L)

            message = "Failed to convert float array to value: $serialized"
            val converted = type.vectorToValue(serialized) as Long

            message = "Received wrong value"
            assertTrue(message, converted == 10L)


            var stuff = FiniteSet(300)

            val number = intArrayOf(10, 23, -1000, 34)

            val first = stuff.add(number[0])

            assertThat(first, equalTo(0))

            message = "Failed to verify duplicate value uses same index"

            assertThat(stuff.add(number[0]), equalTo(0))

            assertThat(stuff.add(number[1]), equalTo(1))


            assertThat(stuff.size, equalTo(2))
            assertThat(stuff.maxSize, equalTo(300))

            var priorSize = stuff.size
            for (i in 0..(stuff.size + 10)){
                val v = Random.nextInt()
                message = "Failed to add value $v"
                val index = stuff.add(v)
                message = "Failed to update set"
                assertTrue(message, stuff.size > priorSize && index != null || stuff.size == stuff.maxSize && stuff.size == priorSize && index == null )
                priorSize = stuff.size

            }



            val valueCogject = stuff.makeValueCogject("item", "Hello")

            assertThat(valueCogject, `is`(notNullValue()))


            message = "Failed to create string Cogject value $valueCogject"
            assertTrue(message, valueCogject?.getValue()== "Hello")


            fun createObjectSequence(cogjectName: String, stepSize: Int, vararg items: Any): Cogject? {
                var root: Cogject? = null
                var prev: Cogject? = null
                items.withIndex().forEach{iv ->
                    val (i, value) = iv;
                    if (prev == null) {
                        root = stuff.makeValueCogject(cogjectName, value)
                        prev = root
                    }
                    else {
                        val nextValue = stuff.makeValueCogject(cogjectName, value)
                        if (nextValue!=null){
                            prev?.interceptor { world, time ->
                                // name is known to be defined here because we stetting it as
                                val lastEntry = world.getLastEntry(cogjectName, time)
                                if (lastEntry != null && lastEntry.updateTime + stepSize < time) {

                                    world.setValue(nextValue, time + 1)

                                }
                                stateValue
                            }
                        }
                        prev = nextValue

                    }
                }
                return root
            }


            fun createTemporalSequence(stepSize: Int, vararg names: String): Cogject? {
                var root: Cogject? = null
                var prev: Cogject? = null

                for ((i, name) in names.withIndex()){
                    val next = stuff.makeValueCogject(name, i)
                    if (prev == null){
                        root = next

                    }
                    else if (next != null){
                        val prevName:String = prev?.name?:""
                        prev?.interceptor{ world, time ->
                            val lastUpdate = world.getLastEntry(prevName, time)
                            if (lastUpdate != null && lastUpdate.updateTime + stepSize > time){
                                world.removeKey(prevName, time+1)
                                world.setValue(next, time +1)
                            }
                            stateValue
                        }
                    }
                    prev = next
                }

                return root
            }

            var world = WorldLine()

            val countCogject = createTemporalSequence(100, "A", "B", "C", "D")

            assertThat(countCogject, `is`(notNullValue()))
            if (countCogject !=null) {
                world.setValue(countCogject, 0)

                val finalState:Map<String, Cogject> = world.run(0L, 2000L, 10L)

                println("Final state after running $finalState")

                message = "Failed to find final value of temporal sequence"

                assertTrue(message, world.hasValue("D"))

                val countDownCogject = createObjectSequence("countdown", 100, 5, 4,3,2,1)

                assertThat(countDownCogject, `is`(notNullValue()))

                world.setValue(countDownCogject!!, 0)

                val afterRerunning = world.run(0, 2000, 10)

                assertThat(afterRerunning, `is`(notNullValue()))

                println("Final value of world: $afterRerunning")
                message = "Failed to find correct final value of countdown key"

                val value = afterRerunning["countdown"]
                assertTrue(message, value != null)

                if (value != null) {
                    val finalValue:Int? = if (value is ValueCogject) value.getValue() as Int  else null
                    assertThat(finalValue, `is`(1))
                }
            }

            // o_>Consistent worlds
            val aTimes = world.consistentWorlds(0L, 0L, arrayOf(stuff.makeValueCogject("A", 0)!! to { cogject: Cogject -> "A" == cogject.name}))
            message = "Failed to receive consistent times"
            assertTrue(message, aTimes.isNotEmpty())

            println("Consistent worlds: ${aTimes}}")


            val moreWorlds = world.consistentWorlds(600L, 1200L, arrayOf(stuff.makeValueCogject("countdown", 4)!! to null))

            println("Consistent worlds 2: ${moreWorlds}")

            // o->Get a random cogject of a name

            val example = world.getAnExampleOf("C", 0L, 2000L)

            assertThat(example, `is`(notNullValue()))
            println("found instance: C = ${example}")
            // o->getCausalFraction

            var causalFraction = world.getCausalInstanceFraction("A", "B", 0)

            println("Fraction $causalFraction")

            val testCauseCogject = stuff.makeValueCogject("cause", 1)

            world.setValue("cause".cog(), 0)

            world.setValue("effect".cog(), 10)

            var causalModel = WorldLine()


            fun String.cog(value: Any): Cogject {
                return stuff.makeValueCogject(this, value) as ValueCogject
            }


            arrayOf("cause-name".cog("cause"), "effect-name".cog("effect"), "cause-time".cog(0), "effect-time".cog(10)).forEach {causalModel.setValue(it, 0)}

            println("Added causal spec to model world: ${causalModel.getState(0)}")

            fun String.query(value: Any): Pair<Cogject, ((Cogject)-> Boolean)?> {
                return Pair(this.cog(value), null)
            }

            val retrieved = causalModel.consistentWorlds(arrayOf("cause-name".query(), "effect-name".query(), "cause-time".query()))

            assertThat(retrieved, `is`(notNullValue()))

            println(retrieved)
            message = "Failed to find all worlds with cause-name"
            assertTrue(message, retrieved.size > 0)

            // o-> Adding capabilities

            val effect = "effect".cog()
            val cause = "cause".cog()

            message = "Effect isn't supposed to have a capability"

            assertTrue(message, cause.getGoalConfidence("effect".goal()) == 0.0F)

            fun getEmpiricalCapability(goal:Goal): Capability {
                return object: Capability(goalCanAchieve = goal) {
                    override fun getConfidence():Float {

                        val name = context?.name
                        if (name != null) {
                            val worldsWithName = causalModel.consistentWorlds(
                                    arrayOf("cause-name".cog(name).query(),
                                            "effect-name".cog(goal?.cogjectWithName?:"").query()))
                            return Math.min(1F, worldsWithName.size.toFloat())
                        }
                        else
                            return 0.0F

                    }
                }
            }


            val emp = getEmpiricalCapability("effect".goal())

            assertThat("Failed to get no confidence before adding to cogject", emp.getConfidence(), equalTo(0.0F))

            cause.addCapability(emp)

            assertThat(emp.getConfidence(), equalTo(1.0F))

            message = "Failed to add confidence to cogject"

            assertTrue(message, cause.getGoalConfidence("effect".goal()) == 1.0F)

            val empCapability = world.getEmpiricalCapability("effect".goal())

            arrayOf<Pair<Cogject, Long>>("cause".cog() to 0L, "effect".cog() to 100L, "cause".cog() to 5000L, "effect".cog() to 5100L, "cause".cog() to 5200L, "effect".cog() to 5400L).forEach { pair -> world.setValue(pair.first, pair.second) }

            val causalStength = world.getCausalStrength("cause", "effect")

            println("Strength: $causalStength")

            val newCause = "cause".cog()

            newCause.addCapability(empCapability)

            val conf = newCause.getGoalConfidence("effect".goal())
            assertTrue(message, conf < 0.4F)

            println("Confidence: $conf")
            // o_>Empirical Capabilities Learning
            world.inferCausalRelationship("D", "C", 0L, 1000L)



        }
        catch (t: Throwable){
            t.printStackTrace()
            assertTrue(message, false)
        }

    }


    @Test fun testWorldLineMerges(){

        var message = ""

        try
        {
            var objectCache = FiniteSet(1000)

            var worldPositiveMemories = WorldLine()
            var shortTermMemory = WorldLine()

            arrayOf("walking", "in-street", "sunny-day").forEach {shortTermMemory.setValue(it.cog(), 0L)}

            var t = 10L
            arrayOf("~walking", "running").forEach { if (it.startsWith("~")) shortTermMemory.removeKey(it.substring(1), t) else shortTermMemory.setValue(it.cog(), t, false)}

            t = 15L
            arrayOf("running", "~in-street", "on-sidewalk", "saw-woman").forEach { if (it.startsWith("~")) shortTermMemory.removeKey(it.substring(1), t) else shortTermMemory.setValue(it.cog(), t, false)}

            t = 20L
            arrayOf("~running", "talking", "on-sidewalk", "saw-woman", "standing").forEach { if (it.startsWith("~")) shortTermMemory.removeKey(it.substring(1), t) else shortTermMemory.setValue(it.cog(), t, false)}

            t = 25L
            arrayOf("on-sidewalk", "~talking", "saw-woman", "kissing").forEach { if (it.startsWith("~")) shortTermMemory.removeKey(it.substring(1), t) else shortTermMemory.setValue(it.cog(), t, false)}

            t = 30L
            arrayOf("on-sidewalk", "~saw-woman", "~kissing").forEach { if (it.startsWith("~")) shortTermMemory.removeKey(it.substring(1), t) else shortTermMemory.setValue(it.cog(), t, false)}

            println("World: ${shortTermMemory.getUpdateTimes().map{t->t to shortTermMemory.getState(t)}}")

            shortTermMemory.clearAllTransactionsInRange(13L, 26L)

            println("World after delete: ${shortTermMemory.getUpdateTimes().map{t->t to shortTermMemory.getState(t)}}")

        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


}