package com.evolved.automata

enum class GoalKey {
    GOAL_NAME, GOAL_EXACT_TIME, GOAL_VALUE, GOAL_FUTURE_TIME, GOAL_PAST_TIMES
}

private fun nop() = "Nothing"

class Goal(val cogjectWithName:String? = null, val cogjectWithValue: Cogject? = null, val observeAtTime: Long? = null, val observeBefore: Long?=null, val observeAfter: Long? = null) {
    val goalsDefined: Map<GoalKey, Any> = HashMap<GoalKey, Any>().apply {
        if (cogjectWithName != null) put(GoalKey.GOAL_NAME, cogjectWithName);
        if (cogjectWithValue!= null) put(GoalKey.GOAL_VALUE, cogjectWithValue);
        if (observeAtTime != null) put(GoalKey.GOAL_EXACT_TIME, observeAtTime);
        if (observeBefore != null) put(GoalKey.GOAL_PAST_TIMES, observeBefore);
        if (observeAfter!= null) put(GoalKey.GOAL_FUTURE_TIME, observeAfter);

    }


    override fun toString(): String{
        return goalsDefined.toString()
    }

    override fun equals(other: Any?): Boolean {
        when {
            other is Goal -> {
                    nop();
                    return cogjectWithName == other.cogjectWithName &&
                    cogjectWithValue == other.cogjectWithValue &&
                    observeAtTime == other.observeAtTime &&
                    observeBefore == other.observeBefore &&
                    observeAfter == other.observeAfter}

            else -> return false
        }
    }

    open fun isSatisfiedBy(cogject: Cogject, observationTime: Long): Boolean {
        val matchesName = (cogjectWithName != null && cogjectWithName == cogject.name) || cogjectWithName == null

        val matchesValue = (cogjectWithValue != null && cogjectWithValue == cogject) || cogjectWithValue == null

        val matchesObservationTime = observeAtTime == null || observeAtTime == observationTime

        val matchesMinObservationTime = observeAfter == null || observeAfter <= observationTime

        val matchesObservationTimeUpperBound = observeBefore == null || observationTime < observeBefore

        return matchesName && matchesValue && matchesObservationTime && matchesMinObservationTime && matchesObservationTimeUpperBound
    }

    /**
     * This means that whenever I'm achieved, other will be achieved
     */
    fun implies(other: Goal): Boolean {
        return other.goalsDefined.all {entry: Map.Entry<GoalKey, Any> ->
            when (entry.key)
            {
                GoalKey.GOAL_NAME -> return cogjectWithName == null || cogjectWithName == other.cogjectWithName
                GoalKey.GOAL_VALUE -> return cogjectWithValue == null || cogjectWithValue == other.cogjectWithValue
                GoalKey.GOAL_EXACT_TIME -> return observeAtTime == other.observeAtTime || observeAfter!=null && observeAfter <= other.observeAtTime!! || observeBefore!=null && observeBefore>other.observeAtTime!!
                GoalKey.GOAL_FUTURE_TIME -> return observeAfter != null &&  observeAfter <= other.observeAfter!!
                GoalKey.GOAL_PAST_TIMES -> return observeBefore != null && observeBefore > other.observeBefore!!

                else -> return false
            }
        }
    }


}




// Can't convert this to a simple lambda function because subclasses may need to define other methods or data
// in order to implement Confidence
interface Confidence {
    fun getConfidence(goal: Goal?= null): Float
}

open class EmpiricalConfidence(var successCount: Int = 0, var favorCount: Int = 0) : Confidence {
    override fun getConfidence(goal: Goal?): Float {
        return if (favorCount == 0) 0F else (successCount as Float)/favorCount
    }

    open fun assertSuccess(){
        successCount++
        favorCount++
    }

    open fun assertFailure() {
        favorCount++
    }
}


open class Favor(val capability: Capability, val processor: Capability.(WorldLine, time: Long, goal: Goal, onComplete: ((FAVOR_STATUS)->Unit)?)->FAVOR_STATUS ,val goal: Goal, val onFinished: ((FAVOR_STATUS)-> Unit)?) {

    fun process(world: WorldLine, time: Long):FAVOR_STATUS {
        return processor(capability, world, time, goal, onFinished)
    }

}

enum class FAVOR_STATUS {
    PENDING, WORKING, SUCCESS, FAILURE
}

open class Capability(val goalCanAchieve: Goal, var confidence: Confidence? = object: Confidence {  override fun getConfidence(goal: Goal?) = 1F}, var requiredGoals: List<Goal>? = null, var context: Cogject? = null) {

    open fun getConfidence(): Float? = confidence?.getConfidence()

    fun requestFavor(world: WorldLine, goalToAchieve: Goal, time: Long,  onFinished: ((FAVOR_STATUS)->Unit)? = null):Favor? {

        if (canAchieveGoal(goalToAchieve, world, time))
            return Favor(this, makeGoalHappen(time), goalToAchieve, onFinished)
        else
            return null
    }

    fun canTheoreticallyAchieveGoal(goal: Goal): Boolean {
        return goalCanAchieve.implies(goal)
    }

    fun canAchieveGoal(goalToAchieve: Goal, world: WorldLine, time: Long): Boolean {

        return hasMetPrerequesites(world, time) && goalCanAchieve.implies(goalToAchieve)
    }

    fun hasMetPrerequesites(world: WorldLine, time: Long):Boolean {
        if (requiredGoals?.isEmpty()?:true){
            return true
        }
        else {
            TODO("Haven't implemented hasMetPrerequesites when requirements are defined")
        }
    }

    open fun makeGoalHappen(requestTime: Long): Capability.(WorldLine, time: Long, goal: Goal, onComplete: ((FAVOR_STATUS)->Unit)?)->FAVOR_STATUS {

        return {world: WorldLine, time: Long, goalToAchieve: Goal, onComplete: ((FAVOR_STATUS)->Unit)? ->
            var status = FAVOR_STATUS.WORKING
            doIt(goalToAchieve, requestTime, time, world)
            if (world.goalIsSatisfied(goalToAchieve, time)) {
                status = FAVOR_STATUS.SUCCESS
                if (onComplete!= null) onComplete(status)
            }
            status
        }
    }

    open fun doIt(goalToAchieve: Goal, requestTime: Long, currentTime: Long, timeLine: WorldLine){

    }

}

open class EmpiricalCapability(goal:Goal, requiredGoals: List<Goal>? = null, var successCount: Int = 0, var favorCount: Int = 0): Capability(goal, EmpiricalConfidence(successCount, favorCount), requiredGoals){

    override fun makeGoalHappen(requestTime: Long): Capability.(WorldLine, time: Long, goal: Goal, onComplete: ((FAVOR_STATUS)->Unit)?)->FAVOR_STATUS {
        TODO("Empirical Capability not fully implemented")
//        return {world: WorldLine, time: Long, goalToAchieve: Goal, onComplete: ((FAVOR_STATUS)->Unit)? ->
//            var status = FAVOR_STATUS.SUCCESS
//
//            //
//            if (onComplete!=null && status in setOf(FAVOR_STATUS.SUCCESS, FAVOR_STATUS.FAILURE))
//                onComplete(status)
//            status
//        }
    }
}