package com.evolved.automata

import com.evolved.automata.nn.util.EnumVector
import com.evolved.automata.nn.util.SetVector
import com.evolved.automata.nn.util.TallyVector
import com.evolved.automata.nn.util.VectorType
import kotlin.random.Random


private fun nop() = "Nothing"



open class Cogject(val stateType: VectorType, var stateValue: FloatArray, var name: String? = null) {

    var processInterceptor: (Cogject.(WorldLine,Long) -> FloatArray)? = null

    var capabilities: MutableSet<Capability> = mutableSetOf()

    var favors = mutableListOf<Favor>()

    override fun toString(): String {
        return "Cogject: <${name?:"unknown"} ${stateType.stringConverter.toString(stateValue)}>"
    }

    fun addCapability(capability: Capability): Cogject {
        capabilities.add(capability)
        return this
    }

    open fun requestFavor(goal: Goal, world: WorldLine, time: Long, onFavorComplete: (FAVOR_STATUS)->Unit):Boolean {
        var foundCapability = false
        capabilities.forEach{cap ->
            if (cap.canTheoreticallyAchieveGoal(goal)) {

                val favor = cap.requestFavor(world = world, time = time, goalToAchieve = goal, onFinished = onFavorComplete)
                if (favor!=null)
                {
                    foundCapability = true
                    favors.add(favor)
                }
            }
        }

        return foundCapability
    }


    fun getGoalConfidence(goal: Goal): Float {
        val capability = capabilities.find{cap -> cap.canTheoreticallyAchieveGoal(goal)}
        return capability?.getConfidence()?:0F
    }

    open fun process(world: WorldLine, processTime: Long) : FloatArray {
        val snap = processInterceptor
        stateValue =if (snap != null) snap(world, processTime) else stateValue
        favors.removeIf{favor: Favor -> val status = favor.process(world, processTime); status !in setOf<FAVOR_STATUS>(FAVOR_STATUS.WORKING, FAVOR_STATUS.PENDING)}
        return processInternal(world, processTime)
    }

    open fun processInternal(world: WorldLine, processTime: Long): FloatArray {
        return stateValue
    }

    open fun getBooleanIntValue(bool: Boolean): Float {
        return if (bool) 1F else 0F
    }

    fun interceptor(interceptor: (Cogject.(WorldLine,Long) -> FloatArray)): Cogject{
        processInterceptor = interceptor
        return this
    }

    override fun equals(other: Any?): Boolean {
        when {
            other is Cogject -> {
                nop();
                return other.name == name &&
                        other.stateType == stateType &&
                        other.stateValue.contentEquals(stateValue)}
            else -> return false
        }
    }
}



open class StateMachineCogject(initialState: Pair<String, StateMachineCogject.(WorldLine, Long) -> FloatArray>,  vararg stateSpecs: Pair<String, (StateMachineCogject.(WorldLine,Long)->FloatArray)> ): Cogject(TallyVector(stateSpecs.size+1), FloatArray(stateSpecs.size+1){0F }) {

    val stringMap: StringToIntConversion

    val states = stateSpecs

    var lastStateTransition: Long = 0

    var prevStateName: String? = null

    var currentStateName: String = initialState.first

    init {
        stringMap = StringToIntConversion().also {me -> stateSpecs.forEach { nameLambdaPair -> me.addString(nameLambdaPair.first) }; me.addString(initialState.first)}

    }

    override fun toString(): String {
        return "StateMachineCogject<$currentStateName>"
    }

    override fun process(world: WorldLine, processTime: Long): FloatArray {
        val stateIndex = stateType.vectorToValue(stateValue) as Int
        stateValue = states[stateIndex].second(this, world, processTime)
        return super.process(world, processTime)
    }

    fun setNextState(name: String, time: Long): FloatArray {
        prevStateName = currentStateName
        lastStateTransition = time
        currentStateName = name
        stateValue = (stateType as TallyVector).valueToVector(stringMap.getStringIndex(name)?:0)
        return stateValue
    }


}


open class IntCogject(value: Int, size: Int, name: String? = null):Cogject(TallyVector(size), FloatArray(size){ i -> if (i >= value) 0F else 1F}, name ) {
    fun getValue(): Int {
        return (stateType as TallyVector).getValue(stateValue)
    }
}

open class RandomIntCogject(name: String):IntCogject(Random.nextInt(0, 10), 10, name)

open class SetCogject(items: Array<String>, set: Set<String>) : Cogject(SetVector(items), FloatArray(items.size){ i -> if (set.contains(items[i])) 1F else 0F}) {

    fun getValue() : Set<String> {
        return HashSet<String>().apply{addAll((stateType as SetVector).getSet(stateValue).map{ it-> it as String})}
    }
}

class UserCogject(name: String) : Cogject(EnumVector(arrayOf("", "")), floatArrayOf(1F, 0F)){

}