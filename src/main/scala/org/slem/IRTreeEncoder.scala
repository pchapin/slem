/*
    Copyright 2010 Timothy Morton
    
    This file is part of Slem.

    Slem is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Slem is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Slem.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.slem

import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.util.Emitter
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class IRTreeEncoder(emitter : Emitter) extends Attribution
{
    import org.slem.IRTree._
    import java.io.FileWriter

    var debugmode = true
    var fileOutputEnabled = true
    var debugtreemode = false
    var fileout = ""
    
    var currentParamNum = 0
    var currentSSA = 0
    var currentGlobalNum = 0
    var currentFuncNum = 0
    var currentLabelNum = 0
    var currentMetadataNum = 0
    
    val metadataname : L_MetadataNode ==> String =
    {
        attr
        {
            case _ =>
            {
                getNewMetadataName()
            }
        }
    }
    
    val labelname : L_Label ==> String =
    {
        attr
        {
            case n : L_Label =>
            {
                if(n.label.size != 0)
                {
                    n.label.label
                }
                else
                {
                    getNewLabelName()
                }
            }
        }
    }
    
    
    val funcname : L_Function ==> String =
    {
        attr
        {
            case n : L_FunctionDefinition =>
            {
                if(n.funcName.size != 0)
                {
                    "@" + n.funcName
                }
                else
                {
                    getNewFuncName()
                }
            }
            case n : L_FunctionDeclaration =>
            {
                if(n.funcName.size != 0)
                {
                    "@" + n.funcName
                }
                else
                {
                    getNewFuncName()
                }
            }
            case _ =>
            {
                getNewFuncName()
            }
        }
    }
    
    val paramName : L_Argument ==> String = 
    {
        attr
        {
            case n : L_Argument =>
            {
                if(n.argName.size == 0)
                {
                    getNewParamName()
                }
                else
                {
                    "%" + n.argName
                }
            }
        }
    }
    
    val ssa : L_Value ==> String =
    {
        attr
        {
            case _ => getNewSSA()
        }
    }
    
    val gvarname : L_GlobalVariable ==> String =
    {
        attr
        {
            case n : L_GlobalVariable => 
            {
                if(n.name.size == 0)
                {
                    getNewGlobalVarName()
                }
                else
                {
                    "@" + n.name
                }
            }
        }
    }
    
    def getNewLabelName() : String =
    {
        currentLabelNum = currentLabelNum + 1
        "block" + (currentLabelNum - 1)
    }
    
    def getNewFuncName() : String =
    {
        currentFuncNum = currentFuncNum + 1
        "@F" + (currentFuncNum - 1)
    }
    
    def getNewGlobalVarName() : String =
    {
        currentGlobalNum = currentGlobalNum + 1
        "@G" + (currentGlobalNum - 1)
    }
    
    def getNewParamName() : String =
    {
        currentParamNum = currentParamNum + 1
        "%param" + (currentParamNum - 1)
    }
    
    def getNewSSA() : String =
    {
        currentSSA = currentSSA + 1
        "%" + (currentSSA - 1)
    }
    
    def getNewMetadataName() : String =
    {
        currentMetadataNum = currentMetadataNum + 1
        "!" + (currentMetadataNum - 1)
    }
    
    def reset(): Unit =
    {
        fileout = ""
        currentParamNum = 0
        currentSSA = 0
    }
    
    def encodeTree(p : L_Program): Unit =
    {
        reset()
        for(module <- p.modules)
        {
            module match
            {
                case m : L_Module =>
                {
                    for(g <- m.globals)
                    {
                        encodeGlobal(g)
                    }
                    for(metnode <- m.metadata)
                    {
                        encodeMetadata(metnode)
                        emitln()
                    }
                }
                case m : L_AsmModule =>
                {
                    emitln("module asm " + '"' + m.asm + '"')
                }
            }
        }
        if(fileOutputEnabled)
        {
            writeFile()
        }
    }
    
    def writeFile(): Unit =
    {
        val fw = new FileWriter("LLVMIR/newprog.ll")
        fw.write(fileout)
        fw.close()    
    }
    
    def encodeGlobal(g : L_Global) : Unit =
    {
        g match
        {
            case v : L_GlobalVariable => encodeGlobalVariable(v)
            case f : L_FunctionReference => encodeGlobal(f.funcPtr)
            case f : L_FunctionDeclaration => encodeFunctionDeclaration(f)
            case f : L_FunctionDefinition => encodeFunctionDefinition(f)
            case _  => {}
        }
    }
    
    def encodeArgument(a : L_Argument): Unit =
    {
        encodeType(a.ty)
        emit(" ")
        if(a.value == null)
        {
            emit(paramName(a))
        }
        else
        {
            encodeValue(a.value)
        }

        for(attrib<-a.attrs)
        {
            emit(" ")
            emit(attrib)
        }
    }
    
    def encodeConstant(c : L_Constant)
    {
        c match
        {
            //Simple Constants
            case n : L_Boolean =>
            {
                if(n.value)
                {
                    emit("true")
                }
                else
                {
                    emit("false")
                }
            }
            case n : L_Int =>
            {
                emit("" + n.value)
            }
            case n : L_Float =>
            {
                emit(n.value)
            }
            case n : L_Double =>
            {
                emit(n.value)
            }
            
            case n : L_FP128          => emit(n.value)
            case n : L_X86FP80        => emit(n.value)
            case n : L_PPCFP128       => emit(n.value)
            
            /* No such thing as a pointer constant except for null ptr
            case n : L_Pointer =>
            {
                //encodeConstant(n.value)
                //emit("*")
                encodeValue(n.value)
                emit("*")
            }
            */
            case n : L_NullPointer =>
            {
                emit("null")
            }
            case n : L_Void =>
            {
            }
            //Complex Constants
            case n : L_PackedStructure =>
            {
                emit("< { ")
                var imax = n.elements.size
                var i = 1
                for(e<-n.elements)
                {
                    encodeType(resultType(e))
                    emit(" ")
                    encodeValue(e)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" } >")
            }
            case n : L_Structure =>
            {
                emit("{ ")
                var imax = n.elements.size
                var i = 1
                for(e<-n.elements)
                {
                    encodeType(resultType(e))
                    emit(" ")
                    encodeValue(e)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" }")
            }
            case n : L_Array =>
            {
                emit("[ ")
                var imax = n.elements.size
                var i = 1
                for(e<-n.elements)
                {
                    encodeType(resultType(e))
                    emit(" ")
                    encodeValue(e)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" ]")
            }
            case n : L_String =>
            {
                emit("c" + '"' + n.s + '"')
            }
            case n : L_ZeroInitialiser =>
            {
                emit("zeroinitializer")
            }
            case n : L_Vector =>
            {
                emit("< ")
                var imax = n.elements.size
                var i = 0
                for(e<-n.elements)
                {
                    encodeType(resultType(e))
                    emit(" ")
                    encodeValue(e)
                    if(i < imax - 1)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" >")
            }
            case n : L_BlockAddress =>
            {
                emit("blockaddress(")
                encodeValue(n.functionTarget)
                emit(", ")
                encodeLabel(n.blockTarget)
                emit(")")
            }
            case _ => emit("Unknown Constant")
        }
    }
    
    def encodeLabel(l : L_Label): Unit =
    {
        emit(labelname(l))
    }
    
    def encodeValue(v : L_Value) : Unit =
    {
        v match {
            case n: L_Argument => emit(paramName(n))
            case n: L_Instruction => emit(ssa(n))
            case n: L_GlobalVariable => emit(gvarname(n))
            case n: L_Constant => encodeConstant(n)
            case n: L_FunctionReference => encodeValue(n.funcPtr)
            case n: L_FunctionDefinition => emit(funcname(n))
            case n: L_FunctionDeclaration => emit(funcname(n))
            case n: L_MetadataString => emit("!" + '"' + n.str + '"')
            case n: L_MetadataNode => emit(metadataname(n))
            case n: L_NamedMetadata => emit("!" + n.name)
            case _ => emit("UnknownValue : " + v)
        }
    }
    
    def encodeMetadata(m : L_BaseMetadata)
    {
        m match
        {
            case n : L_MetadataNode =>
            {
                emit(n->metadataname + " = metadata !{ ")
                var i = 0
                for(mnode <- n.fields)
                {
                    encodeType(resultType(mnode))
                    emit(" ")
                    encodeValue(mnode)
                    if(i < n.fields.size - 1)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit("}")
            }
            case n : L_NamedMetadata =>
            {
                emit("!" + n.name + " = !{ ")
                var i = 0
                for(mnode <- n.fields)
                {
                    //encodeType(mnode->resultType)
                    //emit(" ")
                    encodeValue(mnode)
                    if(i < n.fields.size - 1)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit("}")
            }
        }
    }
    
    def encodeBoundMetadata(b : L_Instruction): Unit =
    {
        if(b.mappedMetadataIdn != null && b.mappedMetadataVal != null)
        {
            emit(", ")
            //encodeType(b.mappedMetadataIdn->resultType)
            //emit(" ")
            encodeValue(b.mappedMetadataIdn)
            emit(" ")
            //encodeType(b.mappedMetadataVal->resultType)
            //emit(" ")
            encodeValue(b.mappedMetadataVal)
        }
    }
    def encodeInstruction(b : L_Instruction): Unit =
    {
        b match
        {
            case n : L_BinOpInstruction =>
            {
                emit(ssa(n))
                emit(" = ")
                emitw(n.instructionString)
                encodeType(resultType(n.LHS))
                emit(" ")
                encodeValue(n.LHS)
                emit(", ")
                encodeValue(n.RHS)
                
            }
            case n : L_ExtractElement =>
            {
                emit(ssa(n))
                emit(" = extractelement ")
                encodeType(resultType(n.vec))
                emit(" ")
                encodeValue(n.vec)
                emit(", i32 ")
                emit("")
                encodeValue(n.idx)
            }
            case n : L_InsertElement =>
            {
                emit(ssa(n))
                emit(" = insertelement ")
                encodeType(resultType(n.vec))
                emit(" ")
                encodeValue(n.vec)
                emit(", ")
                encodeType(resultType(n.elt))
                emit(" ")
                encodeValue(n.elt)
                emit(", i32 ")
                encodeValue(n.idx)
            }
            case n : L_ShuffleVector =>
            {
                emit(ssa(n))
                emit(" = shufflevector ")
                encodeType(resultType(n.v1))
                emit(" ")
                encodeValue(n.v1)
                emit(", ")
                encodeType(resultType(n.v2))
                emit(" ")
                encodeValue(n.v2)
                emit(", ")
                encodeType(resultType(n.mask))
                emit(" ")
                encodeValue(n.mask)
            }
            case n : L_ExtractValue =>
            {
                emit(ssa(n))
                emit(" = extractvalue ")
                encodeType(resultType(n.value))
                emit(" ")
                encodeValue(n.value)
                for(idx<-n.indexes)
                {
                    emit(", ")
                    encodeType(resultType(idx))
                    emit(" ")
                    encodeValue(idx)
                }
            }
            case n : L_InsertValue =>
            {
                emit(ssa(n))
                emit(" = insertvalue ")
                encodeType(resultType(n.value))
                emit(" ")
                encodeValue(n.value)
                emit(", ")
                encodeType(resultType(n.elt))
                emit(" ")
                encodeValue(n.elt)
                emit(", ")
                encodeValue(n.idx)
            }
            case n : L_Alloca =>
            {
                emit(ssa(n))
                emit(" = alloca ")
                encodeType(n.typ)
                if(n.numElements != null)
                {
                   emit(", ")
                   encodeType(resultType(n.numElements))
                   emit(" ")
                   encodeValue(n.numElements)
                }
                if(n.alignment != 0)
                {
                   emit(", align " + n.alignment)
                }
            }
            case n : L_Load =>
            {
                emit(ssa(n))
                emit(" = ")
                if(n.isVolatile)
                {
                    emit("volatile ")
                }
                emit("load ")
                encodeType(n.typ)
                emit(" ")
                encodeValue(n.pointer)
                if(n.alignment != 0)
                {
                    emit(", align " + n.alignment)
                }
                /* Use metadata nodes to support this
                if(n.nonTemporal)
                {
                    emit(", !nontemporal !" + n.nonTempIndex)
                }
                */
            }
            case n : L_Store =>
            {
                if(n.isVolatile)
                {
                    emit("volatile ")
                }
                emitw("store")
                encodeType(resultType(n.value))
                emit(" ")
                encodeValue(n.value)
                emit(", ")
                encodeType(resultType(n.pointer))
                emit(" ")
                encodeValue(n.pointer)
                if(n.alignment != 0)
                {
                    emit(", align " + n.alignment)
                }
                /* Use metadata nodes to support this
                if(n.nonTemporal)
                {
                    emit(", !nontemporal !" + n.nonTempIndex)
                }
                */
            }
            case n : L_GetElementPtr =>
            {
                emit(ssa(n))
                emit(" = getelementptr ")
                if(n.inBounds)
                {
                    emit("inbounds ")
                }
                if(n.pty != null)
                {
                    encodeType(n.pty)
                }
                else
                {
                    encodeType(resultType(n.pval))
                }
                emit(" ")
                encodeValue(n.pval)
                for(ti<-n.typeIndexes)
                {
                    emit(", ")
                    encodeType(resultType(ti))
                    emit(" ")
                    encodeValue(ti)
                    /*
                    encodeType(ti.ty)
                    emit(" ")// + ti.idx)
                    encodeValue(ti.idx)
                    */
                }
            }
            case n : L_ConversionOperation =>
            {
                emit(ssa(n))
                emit(" = " + n.instructionString + " ")
                encodeType(resultType(n.value))
                emit(" ")
                encodeValue(n.value)
                emit(" to ")
                encodeType(n.targetType)
            }
            case n : L_ICMP =>
            {
                emit(ssa(n))
                emit(" = icmp " + n.compCode + " ")
                encodeType(resultType(n.LHS))
                emit(" ")
                encodeValue(n.LHS)
                emit(", ")
                encodeValue(n.RHS)
            }
            case n : L_FCMP =>
            {
                emit(ssa(n))
                emit(" = fcmp " + n.compCode + " ")
                encodeType(resultType(n.LHS))
                emit(" ")
                encodeValue(n.LHS)
                emit(", ")
                encodeValue(n.RHS)
            }
            case n : L_Phi =>
            {
                emit(ssa(n))
                emit(" = phi ")
                encodeType(resultType(n.valueLabels.head.value))
                var i = 0
                for(vlab<-n.valueLabels)
                {
                    emit(" [ ")
                    encodeValue(vlab.value)
                    emit(", %")
                    encodeLabel(vlab.label)
                    emit(" ]")
                    if(i < n.valueLabels.size - 1)
                    {
                      emit(",")
                    }
                    i = i + 1
                }
            }
            case n : L_Select =>
            {
                emit(ssa(n))
                emit(" = select ")
                encodeType(resultType(n.cond))
                emit(" ")
                encodeValue(n.cond)
                emit(", ")
                encodeType(resultType(n.val1))
                emit(" ")
                encodeValue(n.val1)
                emit(", ")
                encodeType(resultType(n.val2))
                emit(" ")
                encodeValue(n.val2)
            }
            case n : L_Call =>
            {
                //Void function call patch - issue 2
                if(n.typ != null)
                {
                    n.typ match {
                        case L_VoidType() => {}
                        case _ => {
                            emit(ssa(n))
                            emit(" = ")
                        }
                    }
                }
                else
                {
                    n.fnptrval->resultType match {
                        case L_VoidType() => {}
                        case _ => {
                            emit(ssa(n))
                            emit(" = ")
                        }
                    }                    
                }
                if(n.tail)
                {
                    emitw("tail")
                }
                emitw("call")
                if(n.callConvention.nonEmpty)
                {
                    emitw(n.callConvention)
                }
                for(ra<-n.returnAttributes)
                {
                    emitw(ra)
                }
                /*
                encodeType(n.typ)
                emit(" ")
                */
                if(n.fnty != null)
                {
                    encodeType(n.fnty)
                }
                else
                {
                    n.fnptrval match
                    {
                        case fref : L_FunctionReference =>
                        {
                           encodeType(resultType(fref.funcPtr))
                        }
                        case _ =>
                        {
                            encodeType(resultType(n.fnptrval))
                        }
                    }
                }
                emit(" ")
                
                encodeValue(n.fnptrval)
                var imax = n.fnargs.size
                var i = 1
                emit("( ")
                for(arg<-n.fnargs)
                {
                    encodeArgument(arg)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" )")
                for(fnattr <- n.fnattrs)
                {
                    emit(" " + fnattr)
                }
            }
            case n : L_Va_Arg =>
            {
                emit(ssa(n))
                emit(" = va_arg ")
                encodeType(resultType(n.argList))
                emit(" ")
                encodeValue(n.argList)
                emit(", ")
                encodeType(n.argType)
            }
            case _ => emit("Unknown Instruction : " + b)
        }
        encodeBoundMetadata(b)
    }
    
    def encodeTerminator(t : L_TerminatorInstruction) = 
    {
        t match
        {
            case n : L_Ret =>
            {
                emitw("ret")
                resultType(n.rvalue) match
                {
                    case n2 : L_VoidType => emit("void")
                    case _ =>
                    {
                        encodeType(resultType(n.rvalue))
                        emit(" ")
                        encodeValue(n.rvalue)
                    }
                }
            }
            case n : L_Br =>
            {
                emit("br label %")
                encodeLabel(n.dest)
            }
            case n : L_BrCond =>
            {
                emitw("br")
                encodeType(resultType(n.cond))
                emit(" ")
                encodeValue(n.cond)
                emit(", label %")
                encodeLabel(n.ifTrue)
                emit(", label %")
                encodeLabel(n.ifFalse)
            }
            case n : L_Switch =>
            {
                emitw("switch")
                encodeType(resultType(n.value))
                emit(" ")
                encodeValue(n.value)
                emit(", label %")
                encodeLabel(n.default)
                emit(" [ ")
                for(valLab<-n.cases)
                {
                    encodeType(resultType(valLab.value))
                    emit(" ")
                    encodeValue(valLab.value)
                    emit(", label %")
                    encodeLabel(valLab.label)
                    emit(" ")
                }
                emit("]")
            }
            case n : L_IndirectBr =>
            {
                emitw("indirectbr")
                encodeType(resultType(n.address))
                emit(" ")
                encodeValue(n.address)
                emit(", [ ")
                var labidx = 0
                for(lab<-n.possibleDestinations)
                {
                    emit("label %")
                    encodeLabel(lab)
                    if(labidx < n.possibleDestinations.size - 1)
                    {
                        emit(",")
                    }
                    emit(" ")
                    labidx = labidx + 1
                }
                emit("]")
            }
            case n : L_Invoke =>
            {
                //Void function call patch - issue 2
                if(n.funcTypePtr != null)
                {
                    n.funcTypePtr match {
                      case L_VoidType() => {}
                      case _ => {
                        emit(ssa(n))
                        emit(" = ")
                      }
                    }
                }
                else
                {
                    n.funcPtrVal->resultType match {
                      case L_VoidType() => {}
                      case _ => {
                        emit(ssa(n))
                        emit(" = ")
                      }
                    }
                }
                emit("invoke ")
                if(n.callConv.nonEmpty)
                {
                    emit(n.callConv)
                    emit(" ")
                }
                for(ra <- n.retAttrs)
                {
                    emit(ra + " ")
                }
                if(n.funcTypePtr != null)
                {
                    encodeType(n.funcTypePtr)
                }
                else
                {
                    encodeType(resultType(n.funcPtrVal))
                }
                emit(" ")
                encodeValue(n.funcPtrVal)
                var imax = n.args.size
                var i = 1
                emit("( ")
                for(arg <- n.args)
                {
                    encodeArgument(arg)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" )")

                for(at<-n.attrs)
                {
                    emit(" " + at)
                }
                emit(" to label %")
                encodeLabel(n.normal)
                emit(" unwind label %")
                encodeLabel(n.unwind)
            }
            case n : L_Unwind =>
            {
                emit("unwind")
            }
            case n : L_Unreachable =>
            {
                emit("unreachable")
            }
            case _ => emit("Unknown Terminator Instruction : " + t)
        }
    }
    
    def encodeBlock(b : L_Block): Unit =
    {
        encodeLabel(b.label)
        emitln(":")
        for(instr <- b.instructions)
        {
            emit("  ")
            encodeInstruction(instr)
            emitln()
        }
        emit("  ")
        encodeTerminator(b.terminator)
        emitln()
        emitln()
    }
    
    def encodeFunctionDefinition(f : L_FunctionDefinition): Unit =
    {
        currentParamNum = 0
        currentSSA = 0
        emitw("define")
        emitw(f.linkage)
        emitw(f.visibilityStyle)
        emitw(f.callConvention)
        for(retattr <- f.returnAttributes)
        {
            emitw(retattr)
        }
        encodeType(f.returnType)
        emit(" ")
        emit(funcname(f))
        emit("(")
        
        var imax = f.arguments.size
        var i = 1
        for(a<- f.arguments)
        {
            encodeArgument(a)
            if(i < imax)
            {
                emit(", ")
            }
            i = i + 1
        }
        
        
        emitw(")")
        for(funcAtt <- f.funcAttributes)
        {
            emitw(funcAtt)
        }
        if(f.section.nonEmpty)
        {
            emitw("section " + '"' + f.section + '"')
        }
        if(f.alignment != 0)
        {
            emitw("align " + f.alignment)
        }
        if(f.garbageCollector.nonEmpty)
        {
            emitw("gc " + '"' + f.garbageCollector + '"')
        }
        emitln("{")
        
        for(b <- f.blocks)
        {
            encodeBlock(b)
        }
        
        emitln("}")
        emitln("")  
    }
    
    def encodeFunctionDeclaration(f : L_FunctionDeclaration): Unit =
    {
        currentParamNum = 0
        currentSSA = 0
        emitw("declare")
        emitw(f.linkage)
        emitw(f.visibilityStyle)
        emitw(f.callConvention)
        for(retattr <- f.returnAttributes)
        {
            emitw(retattr)
        }
        encodeType(f.returnType)
        emit(" ")
        emit(funcname(f))
        emit("(")
        
        var imax = f.arguments.size
        var i = 1
        for(a<- f.arguments)
        {
            encodeType(resultType(a))
            if(i < imax)
            {
                emit(", ")
            }
            i = i + 1
        }
        
        
        emitw(")")
        if(f.alignment != 0)
        {
            emitw("align " + f.alignment)
        }
        if(f.garbageCollector.nonEmpty)
        {
            emitw("gc " + '"' + f.garbageCollector + '"')
        }
        emitln("")  
    }
    
    def encodeType(t : L_Type) : Int =
    {
        t match
        {
            //Basic types
            case n : L_IntType => emit("i" + n.size)
            case n : L_FloatType => emit("float")
            case n : L_DoubleType => emit("double")
            case n : L_FP128Type => emit("fp128")
            case n : L_X86FP80Type => emit("x86fp80")
            case n : L_PPCFP128Type => emit("ppcfp128")
            case n : L_VoidType => emit("void")
            case n : L_MetadataType => emit("metadata")
            case n : L_LabelType => emit("label")
            case n : L_VarArgsType => emit("...")
            
            //Derived types
            case n : L_ArrayType => 
            {
                emit("[" + n.numElements + " x ")
                encodeType(n.elementType)
                emit("]")
            }
            case n : L_FunctionType =>
            {
                encodeType(n.returnType)
                emit("( ")
                var imax = n.parameterList.size
                var i = 1
                for(param<-n.parameterList)
                {
                    encodeType(param)
                    if(i < imax)
                    {
                       emit(", ")
                    }
                    i = i + 1
                }
                emit(" )")
            }
            case n : L_StructureType =>
            {
                emit("{ ")
                var imax = n.fields.size
                var i = 0
                for(field<-n.fields)
                {
                    encodeType(field)
                    if(i < imax - 1)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" }")
            }
            case n : L_PackedStructureType =>
            {
                emit("< { ")
                var imax = n.fields.size
                var i = 1
                for(field<-n.fields)
                {
                    encodeType(field)
                    if(i < imax)
                    {
                        emit(", ")
                    }
                    i = i + 1
                }
                emit(" } >")
            }
            case n : L_PointerType =>
            {
                encodeType(n.pointer)
                emit("*")
            }
            case n : L_VectorType =>
            {
                emit("<" + n.numElements + " x ")
                encodeType(n.elementType)
                emit(">")
            }
            case n : L_OpaqueType =>
            {
                emit("opaque")
            }
            case n : L_UpReferenceType =>
            {
                emit("\\" + n.levels + "*")
            }
            case _ => "Unknown Type : " + t
        }
        0
    }
    
    def encodeGlobalVariable(g : L_GlobalVariable): Unit =
    {
        emit(gvarname(g))
        emit(" =")
        if(g.linkage.nonEmpty)
        emit(" " + g.linkage)
        if(g.addressSpace != 0)
        {
            emit(" addrspace(" + g.addressSpace + ")")
        }
        if(g.isConstant)
        {
            emit(" constant")
        }
        emit(" ")
        encodeType(resultType(g.value))
        emit(" ")
        encodeValue(g.value)
        if(g.section.nonEmpty)
        {
            emit(", section " + '"' + g.section + '"')
        }
        if(g.alignment != 0)
        {
            emit(", align " + g.alignment)
        }
        emitln()
        emitln()
    }
    
    
    
    
    
    def emitln(): Unit =
    {
        emit("\n")
    }
    
    def emitln(s : String): Unit =
    {
        emit(s + "\n")
    }
    
    def emit(s : String): Unit =
    {
        emitter.emit(s)
        if(fileOutputEnabled)
        {
            appendFile(s)
        }
    }
    
    def emitw(s : String): Unit =
    {
        if(s.length > 0)
        {
            emit(s + " ")
        }
    }
    
    def appendFile(s : String): Unit =
    {
        fileout = fileout + s
    }
    
    
}