package com.evolved.automata

import com.evolved.automata.lisp.Environment
import com.evolved.automata.nn.util.EnumVector
import com.evolved.automata.nn.util.SetVector
import com.evolved.automata.nn.util.TallyVector
import com.evolved.automata.nn.util.VectorType
import java.text.SimpleDateFormat
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.math.abs


fun <T> MutableMap<T, Int>.incrementKey(key: T, increment: Int = 1, initialValue: Int = 0):Map<T, Int> {
    put(key, this[key]?:initialValue + increment)
    return this
}


fun <T, K> MutableMap<T, MutableList<K>>.appendValue(key: T, value: K, initialValue: MutableList<K> = mutableListOf<K>(), maxLength: Int? = null): MutableMap<T, MutableList<K>> {
    val newValue = (get(key)?:initialValue).apply{add(value)}

    if (maxLength != null){
        put(key, mutableListOf<K>().apply{addAll(newValue.takeLast(maxLength))})
    }
    else {
        put(key, newValue)
    }
    return this
}

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

    fun getSize() = stringToIntMap.size

    fun getStrings() = stringToIntMap.keys.toSet()
}

data class PlanningStage(val casuse: Cogject, val subGoal: Goal, val favorTime: Long)
data class CogjectTime(val cogject: Cogject, val time:Long)

val UNKNOWN_ITEM_NAME = "UNKNOWN"
val USER_COGJECT_NAME = "USER"
open class WorldLine(var objectCache:FiniteSet = FiniteSet(1000000)) {

    companion object {
        val DELETED = IntCogject(1 , 1, "DELETED")

        private fun findNextTimeIndex(time: Long, timeline:ArrayList<CogjectEntry>, timeInclusive: Boolean = true) : Int? {
            //TODO("Check this again")
            if (timeline.size == 0) {
                return null
            }

            val greatestLowerBound = findIndex(time, timeline)

            if (greatestLowerBound == null) {
                return 0
            }

            if (timeInclusive && greatestLowerBound != null && timeline[greatestLowerBound].updateTime == time)
                return greatestLowerBound

            if (greatestLowerBound!=null && greatestLowerBound < timeline.size - 1) {
                return greatestLowerBound + 1
            }

            return null
        }

        private fun findIndex(time: Long, timeline:ArrayList<CogjectEntry>, allowDeletedP:Boolean = false): Int?
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
            return if (allowDeletedP || timeline[mid].entry != DELETED) mid else null
        }
    }

    data class InstanceMetaDeta (val time: Long, var activeKeys: MutableSet<String>)
    data class CogjectEntry(val updateTime:Long, val entry: Cogject)
    data class Cache(var values: MutableMap<String,  ArrayList<CogjectEntry>> = mutableMapOf(), var solutionRange: LongRange = 0L..0L)

    val state = HashMap<String, ArrayList<CogjectEntry>>()

    private val temporalMetaData = TreeMap<Long, InstanceMetaDeta>()

    val cogjectBuilder:FiniteSet = FiniteSet(1000)

    fun makeSimpleCogject(name: String, value:Any): Cogject? {
        return cogjectBuilder.makeValueCogject(name, value)
    }

    fun say(speech:String, time:Long): Long {
        val tokens = speech.split(' ')
        for ((t, token) in tokens.withIndex()){
            setValue(makeSimpleCogject("speech", token)!!, time + t )
        }
        return tokens.size + time
    }



    fun getEarliestTime():Long? = if (temporalMetaData.isNotEmpty()) temporalMetaData.firstKey() else null

    fun getLastTime():Long? = if (temporalMetaData.isNotEmpty()) temporalMetaData.lastKey() else null

    private var empiricalMetaData: WorldLine? = null

    private var empiricalCapabilities = mutableMapOf<String, (Cogject)->Unit>()

    fun goalIsSatisfied(goal: Goal, time: Long): Boolean {
        return getState(time).values.any{cogject -> goal.isSatisfiedBy(cogject, time)}
    }

    fun everyKeyEveryCreated():List<String> {
        if (state.keys.isEmpty()){
            return listOf()
        }
        val sortedKeys:List<String> = state.keys.sortedBy { key ->
            val list = state?.get(key)
            if (list?.size?:0 == 0){
                -1
            }
            else
                list?.get(0)?.updateTime?:0 }

        return sortedKeys.toList()
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
            return this
        }

        var prior:InstanceMetaDeta = temporalMetaData.get(time) as InstanceMetaDeta

        prior.activeKeys.remove(desc)

        if (prior.activeKeys.size == 0)
            temporalMetaData.remove(time)
        return this
    }



    fun getKeysInChronologicalOrder(time: Long): Set<String> {
        val state = getState(time)
        val sortedKeys:List<String> = state.keys.sortedWith (Comparator<String>{lvalue: String, rvalue:String->
            val (lvage, rvage) = Pair(getCogjectCurrentAge(lvalue, time), getCogjectCurrentAge(rvalue, time))
            if (lvage > rvage){
                -1
            }
            else if (lvage == rvage){
                state[lvalue]!!.name.compareTo(state[rvalue]!!.name)
            }
            else
                1
        })

        return mutableSetOf<String>().apply{addAll(sortedKeys)}.toSet()
    }

    fun getKeysInOldestUpdateOrder(time: Long): List<String> {
        val stateInstance = getState(time)
        val sortedKeys:List<String> = stateInstance.keys.sortedWith (Comparator<String>{lvalue: String, rvalue:String->
            val (lvage, rvage) = Pair(state.get(lvalue)?.get(0)!!.updateTime, state.get(rvalue)?.get(0)!!.updateTime)
            if (lvage < rvage){
                -1
            }
            else if (lvage == rvage){
                stateInstance[lvalue]!!.name.compareTo(stateInstance[rvalue]!!.name)
            }
            else
                1
        })

        return sortedKeys
    }

    data class CogjectInstanceSummary (val keyName: String, val valString:String, val totalLifeTime:Long?, val valueLifeTime: Long?, var deathDay:Long? , val birthdayString:String)

    fun getCogjectMetaData(name: String, time: Long): CogjectInstanceSummary? {
        val history = state[name]
        if (history == null){
            return null
        }

        val index = findIndex(time, history)
        val deleted = index == null && findIndex(time, history, true)!=null
        if (index!=null || deleted){
            val keyName = name
            if (deleted) {
                return CogjectInstanceSummary(keyName, "", getCogjectCurrentAge(name, time - 1), 0, time, "")
            }

            val valueString = history[index!!].entry.toString()
            val totalLifeDurationMs = getCogjectCurrentAge(name, time)
            val valueLifetime = time - history[index].updateTime
            var deathTime:Long?=null
            for (i in index+1 until history.size){
                if (history[i].entry == DELETED){
                    deathTime = history[i].updateTime
                    break
                }
            }
            val birthdayString:String = (SimpleDateFormat("yyyy.MM.dd 'at' HH:mm:ss").format(Date(time - totalLifeDurationMs)))
            return CogjectInstanceSummary(keyName, valueString, totalLifeDurationMs, valueLifetime, deathTime, birthdayString)
        }
        else
            return null
    }

    fun getKeyMetaDataInUpdateOrder(time: Long): Set<String> {
        val stateInstance = getState(time)
        val sortedKeys:List<String> = stateInstance.keys.sortedWith (Comparator<String>{lvalue: String, rvalue:String->
            val (lvage, rvage) = Pair(state.get(lvalue)?.get(0)!!.updateTime, state.get(rvalue)?.get(0)!!.updateTime)
            if (lvage > rvage){
                -1
            }
            else if (lvage == rvage){
                stateInstance[lvalue]!!.name.compareTo(stateInstance[rvalue]!!.name)
            }
            else
                1
        })

        return mutableSetOf<String>().apply{addAll(sortedKeys)}.toSet()
    }

    fun getAllValuesAt(key:String, time:Long): List<Cogject>{
        val history = state[key]
        var out = mutableListOf<Cogject>()
        if (history != null){
            val entry = getLastEntry(key, time)
            if (entry != null){
                if (entry.updateTime!=time){
                    out.add(entry.entry)
                }
                else {
                    for (i in 0 until history.size){
                        if (history[i].updateTime == time){
                            out.add(history[i].entry)
                        }
                        else if (history[i].updateTime > time){
                            break
                        }
                    }
                }
            }
        }
        return out
    }


    fun getUpdateTimes():List<Long> {
        val (start, end) = getEarliestTime() to getLastTime()
        if (start != null && end != null){
            return getUpdateTimes(start, end)
        }
        else
            return listOf()
    }


    fun getUpdateTimes(firstTime: Long, lastTime: Long): List<Long> {
        val items =  temporalMetaData.subMap(firstTime, true, lastTime, true).toList()
        return items.map {p -> p.first}
    }

    fun getUpdateTimes(name: String): List<Long> {
        val (start, end) = getEarliestTime() to getLastTime()
        if (start != null && end != null){
            return getUpdateTimes(name, start, end)
        }
        else
            return listOf()
    }


    fun getAllValuesSinceBirth(name:String, time:Number): List<Cogject> {
        val out = mutableListOf<Cogject>()
        if (hasValue(name, time.toLong())){
            var index = findIndex(time.toLong(), state[name]!!)!!
            val stack = mutableListOf<Cogject>()
            while (index >= 0 && state[name]!![index]?.entry != DELETED){
                stack.add(state[name]!![index]!!.entry)
                index--
            }
            out.addAll(stack.reversed())
        }
        return out
    }



    fun getUpdateTimes(name: String, firstTime: Long, lastTime: Long): List<Long> {
        val items = state[name]?:ArrayList<CogjectEntry>()

        var startIndex = findNextTimeIndex(firstTime, items)?:-1

        var out = mutableListOf<Long>()

        while (startIndex >= 0 && startIndex < items.size && items[startIndex].updateTime <= lastTime && items[startIndex].updateTime>=firstTime){
            out.add(items[startIndex].updateTime)
            startIndex++
        }
        return out.toList()
    }


    fun getEmpiricalCapability(goal: Goal) : Capability {

        return object: Capability(goalCanAchieve = goal) {

            override fun getConfidence(): Float?  {
                val name = context?.name
                if (name != null && goal.cogjectWithName != null) {
                    return getCausalStrength(name, goal.cogjectWithName).toFloat()
                }
                else
                    return 0.0F
            }

        }
    }


    fun consistentWorlds(keyvalues: Array<Pair<Cogject, ((Cogject) -> Boolean)?>>): Map<Long, Map<String, Cogject>> {
        return consistentWorlds(getEarliestTime()?:0, getLastTime()?:0, keyvalues)
    }

    fun consistentWorlds(startTime:Long, endTime: Long, keyvalues: Array<Pair<Cogject, ((Cogject) -> Boolean)?>>): Map<Long, Map<String, Cogject>> {

        var memos = Cache()
        var updateOrder = getPreferredOrder(keyvalues, startTime, endTime, memos)

        var consistentTimes = mutableMapOf<Long, Map<String, Cogject>>()
        if (updateOrder!=null && updateOrder.isNotEmpty()) {
            val (cogject:Cogject, mapper:((Cogject)->Boolean)?) = updateOrder[0]

            val cname = cogject?.name

            fun getValue(name: String, t: Long): Cogject? {
                return memos.values[name]?.find {entry -> entry.updateTime <= t}?.entry
            }

            val others = updateOrder.drop(1)
            for (entry in memos.values[cname]!!) {
                val (time, cogject) = entry
                if (time <= memos.solutionRange.last && time >= memos.solutionRange.start){

                    var map = mutableMapOf<String, Cogject>()
                    consistentTimes[time] = map
                    map[cname] = cogject
                    var invalid = false
                    for (spec in others){
                        val (cogject:Cogject, _) = spec
                        val found = getValue(cogject.name, time)
                        if (found != null)
                            map[cogject.name] = found
                        else {
                            invalid = true
                            break
                        }
                    }

                    if (invalid){
                        consistentTimes.remove(time)
                    }
                }
            }

        }
        return consistentTimes.toMap()

    }

    fun getPreferredOrder(items: Array<Pair<Cogject, ((Cogject) -> Boolean)?>>, startTime:Long, lastTime: Long, memos: Cache = Cache()): Array<Pair<Cogject, ((Cogject) -> Boolean)?>>? {
        var smallestRangeMemo: LongRange = startTime..lastTime

        val comparator = compareBy { item:Pair<Cogject, ((Cogject) -> Boolean)?> ->
            val cog = item.first;
            val updateTimes = memos.values[cog.name];
            if (!updateTimes.isNullOrEmpty()){
                updateTimes.count{entry -> entry.updateTime <= smallestRangeMemo.last && entry.updateTime >= smallestRangeMemo.start}
            }
            else
                Int.MIN_VALUE
        }

        var solutionHeap = PriorityQueue<Pair<Cogject, ((Cogject) -> Boolean)?>>(items.size, comparator)

        for (item: Pair<Cogject, ((Cogject) -> Boolean)?> in items){
            val (requestedCogject, map) = item
            val updates = state[requestedCogject.name]

            var itemsInRangeMemo = memos.values[requestedCogject.name]?:ArrayList<CogjectEntry>()
            memos.values[requestedCogject.name] = itemsInRangeMemo

            val min = smallestRangeMemo.start
            val max = smallestRangeMemo.last

            var newMax: Long? = null
            var newMin:Long? = null
            var newRange:LongRange? = null
            if (updates!=null){
                var index: Int? = findIndex(min, updates)

                if (index == null){
                    index = 0
                    while (index < updates.size){
                        if (updates[index].updateTime >= min && updates[index].updateTime<= max)
                            break;
                        index++
                    }

                    if (index == updates.size)
                        index = null
                }

                while (index != null && index < updates.size && updates[index].updateTime <= max) {
                    val foundCogject = updates[index].entry

                    if (map != null && map(foundCogject) ||
                            requestedCogject == foundCogject)
                    {
                        itemsInRangeMemo.add(updates[index])
                        newMax = Math.max(newMax?: updates[index].updateTime, updates[index].updateTime)
                        newMin = Math.min(newMin?: updates[index].updateTime, updates[index].updateTime)
                        newRange = newMin..newMax
                    }

                    index++
                }
            }

            if (newRange != null) {
                if (newRange.last - newRange.start < max - min) {
                    smallestRangeMemo = newRange
                }
                solutionHeap.add(item)
            }
        }

        if (solutionHeap.size == items.size){
            memos.solutionRange = smallestRangeMemo
            return Array(solutionHeap.size) {  solutionHeap.poll()}
        }
        else
            return null
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

    fun setValue(desc: String, value: Cogject, time: Number, allowDupes: Boolean = true):WorldLine {
        if (!allowDupes && getLastValue(desc, time.toLong()) == value)
            return this
        value.name = desc
        val first = AtomicReference<Boolean>(true)
        val prior = state[desc]?: ArrayList<CogjectEntry>()
        val new = ArrayList<CogjectEntry>(prior.size + 1).also {it -> prior.forEach{oldEntry:CogjectEntry -> if (oldEntry.updateTime >= time.toLong() && first.getAndUpdate{false}) it.add(CogjectEntry( time.toLong(), value)); it.add(oldEntry)}}
        if (first.get())
            new.add(CogjectEntry(time.toLong(), value))
        assertKeyExists(time.toLong(), desc)
        state[desc] = new
        return this
    }



    fun setValue(value: Cogject, time: Long, allowDupes:Boolean = true): WorldLine {
        return setValue(value.name?:UNKNOWN_ITEM_NAME, value, time, allowDupes)
    }

    fun addCogject(value: Cogject, time: Number): WorldLine {
        return setValue(value.name?:UNKNOWN_ITEM_NAME, value, time.toLong(), true)
    }

    fun addValueCogject(name:String, value:Any, time:Number): Cogject {
        val cog = objectCache.makeValueCogject(name, value)!!
        addCogject(cog, time.toLong())
        return cog
    }

    fun <T> getCogjectValue(name:String, time:Number): T? {
        val cog = getState(time.toLong())[name]
        if (cog == null)
            return null
        else {
            return (cog as ValueCogject).getValue() as T
        }
    }

    fun setValue(name:String, value:Any, time: Number = System.currentTimeMillis()): WorldLine{
        return setValue(name, objectCache.makeValueCogject(name, value)!!, time)
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

    fun run(startTime: Long, stopTime: Long, step: Long): Map<String, Cogject> {
        for (time in startTime .. stopTime step step) {
            process(time)
        }
        return getState(stopTime)
    }

    fun getAnExampleOf(name:String, startTime: Long, endTime: Long): Cogject? {
        return state[name]?.filter { entry -> entry.updateTime <= endTime && entry.updateTime >= startTime}?.random()?.entry
    }

    fun getCausalInstanceFraction(cause: String, effect: String, causeTime: Long): Double {
        val effectTime = findNextValueTime(effect, causeTime, true, false)
        val nextCauseTime = findNextValueTime(cause, causeTime+1, true, true)

        if (effectTime == null || nextCauseTime == null)
            return 0.0

        return effectTime.toDouble()/nextCauseTime.toDouble()
    }

    fun getCausalStrength(cause: String, effect: String, causalFractionCutOff: Double = 0.9): Double {
        return getCausalStrength(cause, effect, getEarliestTime()?:0, getLastTime()?:0, causalFractionCutOff)
    }


    fun getCausalStrength(cause: String, effect: String, startTime: Long, endTime:Long, causalFractionCutOff: Double = 0.9): Double {
        val causeTimes = getUpdateTimes(cause, startTime, endTime)
        if (causeTimes.size > 0) {
            return causeTimes.filter {t -> val f = getCausalInstanceFraction(cause, effect, t); f > 0 && f < causalFractionCutOff}.count().toDouble()/causeTimes.size
        }
        return 0.0
    }

    fun inferCausalRelationship(firstName: String, secondName: String, startTime: Long, endTime:Long) : WorldLine {
        val (forwardCausality, backwardCausality) = Pair<Double, Double>(getCausalStrength(firstName, secondName, startTime, endTime), getCausalStrength(secondName, firstName, startTime, endTime))

        if (forwardCausality < 0.9F && forwardCausality > 0) {
            empiricalCapabilities.put(firstName) {cogject: Cogject ->
                cogject.addCapability(getEmpiricalCapability(Goal(cogjectWithName = secondName)))
            }
        }

        if (backwardCausality < 0.9F && backwardCausality > 0) {
            empiricalCapabilities.put(secondName) {cogject: Cogject ->
                cogject.addCapability(getEmpiricalCapability(Goal(cogjectWithName = firstName)))
            }
        }
        return this
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

    private fun findLastValue(searchTime: Long, history:ArrayList<CogjectEntry>): CogjectEntry? {

        val priorIndex = findIndex(searchTime, history)
        if (priorIndex == null)
            return null
        val time = history[priorIndex].updateTime
        var upperIndex = priorIndex
        var lowerIndex = priorIndex
        while (upperIndex < history.size-2 && history[upperIndex+1].updateTime == time && history[upperIndex+1].entry != DELETED) upperIndex++
        while (lowerIndex > 0 && history[lowerIndex-1].updateTime == time && history[lowerIndex-1].entry != DELETED) lowerIndex--

        return history[lowerIndex]
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

                for (i in nextIndex until history.size){
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


    fun getCogjectCurrentAge(name: String, time:Long): Long{
        if (hasValue(name, time)){
            val values = state[name]!!
            val index:Int = findIndex(time, values)!!
            var totalTime = 0L
            for (i in index downTo 0){
                if (values[i].entry!= DELETED) {
                    if (i == index){
                        totalTime+=time - values[i].updateTime
                    }
                    else {
                        totalTime+=values[i+1].updateTime - values[i].updateTime
                    }
                }
                else
                    break
            }

            return totalTime
        }
        else {
            return 0L
        }
    }

    fun getDeathTime(name: String, time:Long): Long? {
        if (hasValue(name, time)){
            val values = state[name]!!
            val index:Int = findIndex(time, values)!!
            for (i in index until values.size){
                if (values[i].entry== DELETED) {
                    return values[i].updateTime
                }

            }
        }
        return null
    }


    fun clearAllTransactionsInRange(startTime: Long, endTime:Long): WorldLine {
        return clearAllTransactionsInRange(startTime, endTime, state.keys.toTypedArray())
    }

    fun clearAllTransactionsInRange(startTime: Long, endTime:Long, names: Array<String>): WorldLine {
        val deleteDeleteSet = names.toSet()

        for (name in names){
            val history = state[name]
            if (history != null){
                var newHistory = ArrayList<CogjectEntry>()
                for (value in history){
                    if (value.updateTime < startTime || value.updateTime> endTime)
                        newHistory.add(value)
                }
                if (newHistory.size>0){
                    state[name] = newHistory
                }
                else
                    state.remove(name)
            }
        }

        if (deleteDeleteSet.isNotEmpty())
            getUpdateTimes(startTime, endTime).forEach{t ->
                var meta = temporalMetaData.get(t)?.activeKeys
                if (meta != null){
                    for (k in deleteDeleteSet)
                        meta.remove(k)
                    if (meta.size == 0) {
                        temporalMetaData.remove(t)
                    }
                }}
        return this
    }

    fun removeLatestValue(desc: String, time:Long): Cogject? {
        var history = state[desc]

        if (history != null){
            var priorIndex = findIndex(time, history)
            if (priorIndex != null) {
                var upperIndex = priorIndex
                var lowerIndex = priorIndex
                while (upperIndex< history.size-2 && history[upperIndex+1].updateTime == time && history[upperIndex+1].entry != DELETED) upperIndex++
                while (lowerIndex > 0 && history[lowerIndex-1].updateTime == time && history[lowerIndex-1].entry != DELETED) lowerIndex--

                val numRemainingAfterDelete = upperIndex - lowerIndex
                for (i in lowerIndex until history.size){
                    val t = history[i].updateTime
                    val data:InstanceMetaDeta? = temporalMetaData[t]
                    if (data!=null && numRemainingAfterDelete == 0){
                        data.activeKeys.remove(desc)
                        if (data.activeKeys.size == 0)
                        {
                            temporalMetaData.remove(t)
                        }
                    }
                    if (i < history.size-1 && history[i+1].entry == DELETED){
                        break
                    }
                }

                val returnV = history[lowerIndex]
                history.removeAt(lowerIndex)
                return returnV.entry
            }
        }
        return null
    }

    fun removeAllValues(desc: String) : Boolean{
        if (state.containsKey(desc)){
            state.remove(desc)
            temporalMetaData.keys.forEach{t ->
                if (temporalMetaData[t]!!.activeKeys.remove(desc) && temporalMetaData[t]!!.activeKeys.isEmpty()) {
                    temporalMetaData.remove(t)
                }

            }
            return true
        }
        return false
    }

    @Deprecated("replaced by [expireKey]")
    fun removeKey(desc: String, time: Long) :Boolean {
        return expireKey(desc, time)
    }

    fun expireKey(desc: String, time: Number) :Boolean{
        assertKeyDoesNotExist(time.toLong(), desc)
        val history = state[desc]
        if (history == null || findIndex(time.toLong(), history) == null) {
            return false
        }
        setValue(desc, DELETED, time)
        return true
    }



    fun hasValue(desc: String, time: Number): Boolean {
        return state[desc]!=null && findIndex(time.toLong(), state[desc] as ArrayList<CogjectEntry>) != null
    }

    fun getState():Map<String, Cogject> {
        val lastTime  = getLastTime()
        if (lastTime != null){
            return getState(lastTime)
        }
        else
            return mapOf<String, Cogject>()
    }

    fun getState(time: Int): Map<String, Cogject> = getState(time.toLong())

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

    fun process(keys: Collection<String>, time: Long, ignoreKeys:Set<String> = setOf()) {
        val activekeys = getActiveKeys(time)
        keys.filter { it -> activekeys.contains(it) && !ignoreKeys.contains(it)}.forEach{key ->  getLastValue(key, time)?.process(this, time)}
    }

    fun getActiveKeys(time: Long): Set<String> {
        return mutableSetOf<String>().also {me -> state.entries.filter { entry -> hasValue(entry.key, time)}.forEach{entry -> me.add(entry.key)} } .toSet()
    }

    fun getAllNarratives(startTime:Long, endTime:Long, offset:Long): MutableList<MutableList<CogjectEntry>> {


        fun isCloseTo(v:Long?, w:Long?, delta:Long): Boolean {
            if (v == null || w == null)
                return false
            return abs(v - w) <= delta
        }

        // The time bounds, starting near [startTime], where a given cogject
        // has a constant value.  If there is no upper bound for this value, the
        // the cogject is assumed to have this value indefinitely
        fun getNextValueBounds(upTimes:List<Long>): Pair<Long, Long?>? {
            for ((i, time) in upTimes.withIndex()){
                if (isCloseTo(time , startTime, offset)){
                    if (i < upTimes.lastIndex){
                        return Pair(time, upTimes[i+1])
                    }
                    else
                        return Pair(time, null)
                }
            }
            return null
        }

        var stateMap = getState(startTime)
        val output:MutableList<MutableList<CogjectEntry>> = mutableListOf<MutableList<CogjectEntry>>()

        val initialUpdateTimes = getUpdateTimes(startTime - offset, endTime)
        if (initialUpdateTimes.isEmpty()){
            return output
        }
        if (stateMap.isEmpty()){
            // Try to find cogjects with update times nearby
            val around = getNextValueBounds(initialUpdateTimes)
            if (around != null)
                stateMap = getState(around.first)
        }

        for (name: String in stateMap.keys){
            val updateTimes = getUpdateTimes(name, startTime - offset, endTime)
            val bounds = getNextValueBounds(updateTimes)
            if (updateTimes.size == 1){ // This means the single update time is either the death
                // time of [key] or the last time the cogject's value was updated
                if (bounds != null){
                    output.add(mutableListOf(CogjectEntry(bounds.first, stateMap[name]!!)))
                }
                return output
            } else if (updateTimes.size > 1){
                if (bounds!=null){
                    val valueEndTime = bounds.second
                    if (valueEndTime != null){
                        if (isCloseTo(valueEndTime, endTime, offset)){
                            // This means the last time of [key] spans the entire interval
                            output.add(mutableListOf(CogjectEntry(bounds.first, stateMap[name]!!)))
                        }
                        else if (valueEndTime < endTime){
                            val remaining = getAllNarratives(valueEndTime, endTime, offset)
                            for (remainingNarrative in remaining){
                                output.add(mutableListOf(CogjectEntry(bounds.first, stateMap[name]!!)).apply{addAll(remainingNarrative)} )
                            }
                        }
                    }

                }
            }

        }
        return output
    }
}