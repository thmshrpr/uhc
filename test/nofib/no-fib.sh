#!/bin/sh
#
###############################################################################
# Configuration
###############################################################################

# Default targets for the parameters
#
DEF_TYPE="normal"
DEF_TARGET="all"
DEF_HC="../../bin/8/ehc"
DEF_EHC_FLAGS="-clexe --verbose=0"
DEF_GHC_FLAGS="-O2"

# Haskell main file for each benchmark
#
MAIN_FILE="Main.hs"

# params file for each benchmark
#
PARAMS_FILE="params"

# Haskell file containing the EHC prelude, which should be prepended to the
# benchmark in case of EHC
#
EHC_PRELUDE_FILE="tools/IntListPreludeFuncs.hs"

# The two template representations in the benchmark files
#
ARGS_TEMPLATE="<ARGS>"
PRINT_INT_TEMPLATE="<PRINT_INT>"

GHC_PRINT_INT="print $ "
EHC_PRINT_INT=""

LOG_FILE="no-fib.log"

###############################################################################
# End of configuration
###############################################################################

###############################################################################
# Local variables, do not modify!
###############################################################################

BENCHMARK_FILE=""

###############################################################################
# End of local variables
###############################################################################

###############################################################################
# Functions 
###############################################################################

# Function to instantiate the template argument ARGS 
#
function replace_args {

  BENCHMARK_DIR=${BENCHMARK_FILE%/*}
  ARGS=$BENCHMARK_DIR/$PARAMS_FILE
  
  COUNTER=1
  source $ARGS
  
  case "$TYPE" in

    "normal" )
      ARGS=$ARGS_NORMAL
      ;;
    "fast" )
      ARGS=$ARGS_FAST
      ;;
    "slow" )
      ARGS=$ARGS_SLOW
      ;;
    * )
      echo "Unknown benchmark type"
      exit 1
      ;;
  esac

  SED_EXPR=" "
  echo "ARGS: $ARGS"
  for i in $ARGS
  do 
    SED_EXPR="$SED_EXPR; s/<ARG$COUNTER>/$i/g"
    let COUNTER=COUNTER+1  
  done

  sed -e "$SED_EXPR" ${BENCHMARK_FILE} > \
    $BENCHMARK_DIR/tmp.$$
  mv $BENCHMARK_DIR/tmp.$$ $BENCHMARK_FILE
}

# Function to instantiate the template argument PRINT_INT
#
function replace_printInt {

  BENCHMARK_DIR=${BENCHMARK_FILE%/*}
  case "${HC##*/}" in

    "ehc" )
      FUNC=$EHC_PRINT_INT
      ;;
    "ghc" )
      FUNC=$GHC_PRINT_INT
      ;;
    * )
      echo "Compiler not supported, please add it to printInt"
      exit 1 
      ;;
  esac
  sed -e "s/${PRINT_INT_TEMPLATE}/${FUNC}/g" ${BENCHMARK_FILE} > \
    $BENCHMARK_DIR/tmp.$$
  mv $BENCHMARK_DIR/tmp.$$ $BENCHMARK_FILE
}  

# Function to make the benchmarkfile if needed
#
function create_benchmarkfile {
 
  BENCHMARK_FILE=$1
  BENCHMARK_DIR=${BENCHMARK_FILE%/*}
  
  mkdir $BENCHMARK_DIR/build
  cp $BENCHMARK_DIR/*.hs $BENCHMARK_DIR/params $BENCHMARK_DIR/build/
  BENCHMARK_FILE=$BENCHMARK_DIR/build/$MAIN_FILE

  if [ "${HC##*/}" = "ehc" ]; then

    FILE_INCL_PRELUDE="${BENCHMARK_FILE%.hs}_plus_prelude.hs"
    cat $BENCHMARK_FILE $EHC_PRELUDE_FILE > $FILE_INCL_PRELUDE
    rm $BENCHMARK_FILE
    BENCHMARK_FILE=$FILE_INCL_PRELUDE
  fi
}

function clean_up {
  BENCHMARK_DIR=${BENCHMARK_FILE%/*}
  rm -rf $BENCHMARK_DIR
}  

function run {
  SUITE=$1
  BENCHMARK="${2}"
  echo "" > $LOG_FILE
  for i in `ls $SUITE/${BENCHMARK}/$MAIN_FILE`;
  do
    CURR_MAIN_FILE=$i
    echo "--- RUNNING no-fib: $CURR_MAIN_FILE ---"
    echo ""
    create_benchmarkfile $CURR_MAIN_FILE
    replace_args
    replace_printInt
    
    OBJFILE=${BENCHMARK_FILE%.hs}
    case "${HC##*/}" in 
      
      "ghc" )
        EFLAGS="-o $OBJFILE"
        ;;
       *)
        ;;
    esac
    echo "$HC $FLAGS $EFLAGS $BENCHMARK_FILE"
    $HC $FLAGS $EFLAGS $BENCHMARK_FILE > $LOG_FILE 2>&1
    ./$OBJFILE
    clean_up
    echo "" 
    echo "--- DONE no-fib: $CURR_MAIN_FILE ---"
  done
}
###############################################################################
# End of functions 
###############################################################################

###############################################################################
# Main code 
###############################################################################

# Reinitialize the logfile
#
echo "" > $LOG_FILE

# Variable to store if we need to save EHC
# Read the parameters and overwrite the Default values
TARGET=$DEF_TARGET
if [ "$1" != "" ]; then
  TARGET=$1
fi

TYPE=$DEF_TYPE
if [ "$2" != "" ]; then
  TYPE=$2
fi

HC=$DEF_HC
if [ "$3" != "" ]; then
  HC=$3
fi

case "${HC##*/}" in

  "ehc" )
    FLAGS=$DEF_EHC_FLAGS
    ;;
  "ghc" )
    FLAGS=$DEF_GHC_FLAGS
    ;;
  * )
    echo "Compiler not supported, please add it to printInt"
    exit 1 
    ;;
esac
if [ "$4" != "" ]; then
  FLAGS=$4
fi

# $TARGET can be either a whole suite or just one test
# Split to find out
INDEX_SLASH=`expr index "$TARGET" /`
SUITE=${TARGET%/*}
BENCHMARK=${TARGET#*/}
if [ $INDEX_SLASH -eq 0 ]; then
  BENCHMARK="*"
fi

case "$SUITE" in

  "all" )
  run "imaginary" "*"
  run "spectral" "*"
  run "real" "*"
  ;;

  "imaginary" )
  run $SUITE "${BENCHMARK}" 
  ;;

  "spectral" )
  run $SUITE "${BENCHMARK}"
  ;;

  "real" )
  run $SUITE "${BENCHMARK}" 
  ;;

  "help" | * )
  echo "Usage: no-fib [target/benchmark] [type] [compiler] [compilerflags]"
  echo "       target:         all|imaginary|spectral|real|help   [all]"
  echo "       type:           slow|normal|fast                   [normal]"
  echo "       compiler:       path to the compiler executable    [$DEF_HC]"
  echo "       compilerflags:  flags passed to the compiler       [$DEF_FLAGS]"
  ;;
esac 
