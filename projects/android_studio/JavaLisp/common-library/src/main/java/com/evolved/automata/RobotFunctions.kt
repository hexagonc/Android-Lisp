package com.evolved.automata

import com.evolved.automata.nn.util.EnumVector
import com.evolved.automata.nn.util.SetVector
import com.evolved.automata.nn.util.TallyVector
import com.evolved.automata.nn.util.VectorType
import com.sun.org.apache.xml.internal.utils.IntVector
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet



class StringToIntConversion(val initialValue:Int = 0) {
    var stringToIntMap = mutableMapOf<String, Int>()
    var IntToStringMap = mutableMapOf<Int, String>()

    fun addString(name: String): StringToIntConversion {
        val index = initialValue + stringToIntMap.size
        stringToIntMap.put(name, index)
        IntToStringMap.put(index, name)
        return this
    }

    fun getStringIndex(name: String):Int? {
        return stringToIntMap[name]
    }

    fun getIntString(index: Int): String? {
        return IntToStringMap[index]
    }

}

data class PlanningStage(val casuse: Cogject, val subGoal: Goal, val favorTime: Long)
data class CogjectTime(val cogject: Cogject, val time:Long)

val UNKNOWN_ITEM_NAME = "UNKNOWN"
open class WorldLine(var stringNames:StringToIntConversion = StringToIntConversion(), val userCogjectName: String = "") {

    companion object {
        val DELETED = Cogject(EnumVector(arrayOf("true", "false")), floatArrayOf(1F,0F)).apply { name = "DELETED"}

        private fun findNextTimeIndex(time: Long, timeline:ArrayList<CogjectEntry>, timeInclusive: Boolean = true) : Int? {
            if (timeline.size == 0) {
                return null
            }

            val greatestLowerBound = findIndex(time, timeline)

            if (timeInclusive && greatestLowerBound != null && timeline[greatestLowerBound].updateTime == time)
                return greatestLowerBound

            if (greatestLowerBound!=null && greatestLowerBound < timeline.size - 1) {
                return greatestLowerBound + 1
            }

            return null
        }

        private fun findIndex(time: Long, timeline:ArrayList<CogjectEntry>): Int?
        {
            if (timeline.size == 0 || timeline[0].updateTime > time) {
                return null
            }

            var (bottom, top ) = Pair(0, timeline.size)

            var mid: Int
            do {
                mid = (top + bottom)/2
                if (timeline[mid].updateTime == time) {
                    break
                }
                else if (timeline[mid].updateTime > time) {
                    top = mid
                }
                else {
                    bottom = mid
                }
            } while (mid != (top + bottom)/2)
            return if (timeline[mid].entry != DELETED) mid else null
        }
    }

    data class InstanceMetaDeta (val time: Long, var activeKeys: MutableSet<String>)
    data class CogjectEntry(val updateTime:Long, val entry: Cogject)

    val state = HashMap<String, ArrayList<CogjectEntry>>()

    private val temporalMetaData = TreeMap<Long, InstanceMetaDeta>()

    fun goalIsSatisfied(goal: Goal, time: Long): Boolean {
        return getState(time).values.any{cogject -> goal.isSatisfiedBy(cogject, time)}
    }

    fun planRouteToGoal(time: Long, sourceCogject: Cogject, goal: Goal): List<PlanningStage>? {

        var path = mutableListOf<PlanningStage>()

        if (goal.cogjectWithName != null) {
            val startEntry = getLastEntry(goal.cogjectWithName, time)
            if (startEntry == null)
                return path
            var currentGoals = listOf<Goal>(Goal(cogjectWithName = goal.cogjectWithName, observeAtTime = startEntry.updateTime))

            val searchPeriod = 20L
            while (currentGoals.isNotEmpty()){
                var newGoals = mutableListOf<Goal>()
                for (goal in currentGoals) {
                    for (causeTime in findCapabilities(goal, goal.observeAtTime!!, maxSearchPeriod = searchPeriod)){
                        path.add(PlanningStage(causeTime.cogject, goal, causeTime.time))
                        if (causeTime.cogject.name == sourceCogject.name!!){
                            return path
                        }
                        newGoals.add(Goal(cogjectWithName = causeTime.cogject.name, observeAtTime = causeTime.time))
                    }
                }
                currentGoals = newGoals
            }
        }

        return path
    }



    fun planRouteToGoal(time: Long, goal: Goal): List<PlanningStage>? {
        for (cogject in getState(time).map {entry -> entry.value}){
            val plan = planRouteToGoal(time, cogject, goal)
            if (plan!=null){
                return plan
            }
        }
        return null
    }


    private fun assertKeyExists(time: Long, desc: String): WorldLine {
        if (!temporalMetaData.containsKey(time)) {
            temporalMetaData.put(time, InstanceMetaDeta(time, mutableSetOf()))
        }

        temporalMetaData.get(time)?.activeKeys?.add(desc)
        return this
    }

    private fun assertKeyDoesNotExist(time: Long, desc: String) : WorldLine {
        if (!temporalMetaData.containsKey(time)) {
            temporalMetaData.put(time, InstanceMetaDeta(time, mutableSetOf()))
        }
        var prior:InstanceMetaDeta = temporalMetaData.get(time) as InstanceMetaDeta

        prior.activeKeys?.remove(desc)
        return this
    }

    fun getUpdateTimes(firstTime: Long, lastTime: Long): List<Long> {
        val items =  temporalMetaData.subMap(firstTime, true, lastTime, true).toList()
        return items.map {p -> p.first}
    }


    fun getGroupedUpdateInstances(key: String): List<List<Long>> {
        var out = mutableListOf<List<Long>>()
        var segment:MutableList<Long>? = mutableListOf()

        state[key]?.forEachIndexed {index, entry ->

            if (entry.entry == DELETED){
                if (segment != null) {
                    out.add(segment?.toList() as List<Long>)
                }
                segment = mutableListOf()
            }
            else {
                segment?.add(entry.updateTime)
            }
        }
        if (segment != null && segment?.size?:0 > 0) {
            out.add(segment?.toList() as List<Long>)
        }
        return out.toList()
    }

    fun getInstancesBetween(start: Long, end: Long): Set<Long> {
        return  temporalMetaData.subMap(start, true, end, true).keys.toSet()
    }

    fun setValue(desc: String, value: Cogject, time: Long):WorldLine {
        value.name = desc
        val first = AtomicReference<Boolean>(true)
        val prior = state[desc]?: ArrayList<CogjectEntry>()
        val new = ArrayList<CogjectEntry>(prior.size + 1).also {it -> prior.forEach{oldEntry:CogjectEntry -> if (oldEntry.updateTime >= time && first.getAndUpdate{false}) it.add(CogjectEntry( time, value)); it.add(oldEntry)}}
        if (first.get())
            new.add(CogjectEntry(time, value))
        assertKeyExists(time, desc)
        state[desc] = new
        return this
    }

    fun setValue(value: Cogject, time: Long): WorldLine {
        return setValue(value.name?:UNKNOWN_ITEM_NAME, value, time)
    }

    fun getLastValue(desc: String, time: Long) : Cogject? {
        val history = state[desc]
        if (history != null){
            return findLastValue(time, history)?.entry
        }
        return null
    }

    fun getLastEntry(desc: String, time: Long): CogjectEntry? {
        val history = state[desc]
        if (history != null){
            return findLastValue(time, history)
        }
        return null
    }


    /**
     * Finds some Cogjects from time [goalCogjectName] - [maxSearchPeriod] that are capable of achieving.  This
     * algorithm tries to find solutions amongst the oldest and most recent times to minimize execution time
     * [goal]
     */
    fun findCapabilities(goal: Goal, goalCogjectTime: Long, maxSearchPeriod: Long): List<CogjectTime> {

        var possibleCauses = mutableListOf<CogjectTime>()

        val searchTimes = goalCogjectTime downTo (goalCogjectTime - maxSearchPeriod)

        for (time in searchTimes) {
            possibleCauses.addAll( getContemporaryCauses(goal, time).map{c -> CogjectTime(c, time)})
            if (possibleCauses.isNotEmpty()){
                break
            }
        }

        return possibleCauses
    }

    /**
     * Find all cogjects at the current time slice that are capable of causing [goal]
     */
    fun getContemporaryCauses(goal: Goal, currentTime: Long): List<Cogject> {
        return getState(currentTime).values.filter{ cogject -> cogject.getGoalConfidence(goal)>0}.sortedBy { cogject -> cogject.getGoalConfidence(goal) }
    }

    private fun findLastValue(time: Long, timeline:ArrayList<CogjectEntry>): CogjectEntry? {
        val entryIndex = findIndex(time, timeline)
        return if (entryIndex!=null)timeline[entryIndex] else null
    }

    fun findNextValue(key: String, time: Long, ignoreRemovalBoundary: Boolean = true, timeInclusive: Boolean = true) : Cogject? {
        return findNextValueSpec(key, time, ignoreRemovalBoundary)?.entry
    }

    fun findNextValueTime(key: String, time: Long, ignoreRemovalBoundary: Boolean = true, timeInclusive: Boolean = true) : Long? {
        return findNextValueSpec(key, time, ignoreRemovalBoundary)?.updateTime
    }

    fun findNextValueSpec(key: String, time: Long, ignoreRemovalBoundary: Boolean = true, timeInclusive: Boolean = true) : CogjectEntry? {

        val history = state[key]
        if (history !=null && history.size > 0) {
            if (time < history[0].updateTime) {
                for (entry in history) {
                    if (entry.entry != DELETED)
                        return entry
                    else if (ignoreRemovalBoundary) {
                        continue
                    }
                    else
                        break
                }
                return null
            }

            val nextIndex = findNextTimeIndex(time, history, timeInclusive)
            if (nextIndex != null) {

                for (i in nextIndex..history.size){
                    if (history[i].entry != DELETED)
                        return history[i]
                    else if (ignoreRemovalBoundary) {
                        continue
                    }
                    else
                        break
                }
            }
        }
        return null
    }

    fun hasValue(desc:String) : Boolean {
        return hasValue(desc, System.currentTimeMillis())
    }

    fun removeLatestValue(desc: String, time:Long): Boolean {
        var history = state[desc]

        if (history != null){
            val priorIndex = findIndex(time, history)
            if (priorIndex != null)
                return history.removeAt(priorIndex)!=null
        }
        return false
    }

    fun removeAllValues(desc: String) : Boolean{
        if (state.containsKey(desc)){
            state.remove(desc)
            return true
        }
        return false
    }

    fun removeKey(desc: String, time: Long) :Boolean {
        assertKeyDoesNotExist(time, desc)
        val history = state[desc]
        if (history == null || findIndex(time, history) == null) {
            return false
        }
        setValue(desc, DELETED, time)
        return true
    }

    fun hasValue(desc: String, time: Long): Boolean {
        return state[desc]!=null && findIndex(time, state[desc] as ArrayList<CogjectEntry>) != null
    }

    fun getState(time: Long): Map<String, Cogject> {
        var map = mutableMapOf<String, Cogject>()


        var validKeys = mutableSetOf<String>()
        var metaData = temporalMetaData[time]?:InstanceMetaDeta(time, validKeys)
        metaData.activeKeys = validKeys

        state.entries.filter { entry -> hasValue(entry.key, time) }.forEach{entry ->  validKeys.add(entry.key); map.put(entry.key, getLastValue(entry.key,time) as Cogject)}

        return map.toMap()
    }

    fun process(time: Long) {
        process(getActiveKeys(time), time)
    }

    fun process(keys: Collection<String>, time: Long) {
        val activekeys = getActiveKeys(time)
        keys.filter { it -> activekeys.contains(it)}.forEach{key ->  getLastValue(key, time)?.process(this, time)}
    }

    fun getActiveKeys(time: Long): Set<String> {
        return mutableSetOf<String>().also {me -> state.entries.filter { entry -> hasValue(entry.key, time)}.forEach{entry -> me.add(entry.key)} } .toSet()
    }
}