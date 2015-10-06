#!/bin/sh

ROOT=`pwd`
KRDF=${ROOT}/dist/build/krdf/krdf
TMPDIR=""
HERMIT_DIST=http://www.hermit-reasoner.com/download/current/HermiT.zip
HERMIT=${ROOT}/dist/hermit/HermiT.jar

error () {
    echo
    echo "*****"
    echo "$*"
    echo "*****"
    echo
    exit 255
}

cleanup () {
    if test -d "${TMPDIR}"; then
	rm -r "${TMPDIR}"
    fi
}

if ! test -x ${KRDF}; then
    error "The krdf tool has not been built. Run 'cabal build' first."
fi

if ! test -d ${ROOT}/dist/hermit; then
(
    if ! which java; then
	error "Unfortunately we need Java to run HermiT for this test"
    fi 
    mkdir -p ${ROOT}/dist/hermit
    cd ${ROOT}/dist/hermit
    wget ${HERMIT_DIST} || error "Error downloading HermiT"
    unzip HermiT.zip || error "Error decompressing HermiT"
)
fi

trap cleanup EXIT INT TERM HUP
export TMPDIR=`mktemp -d`

cat > ${TMPDIR}/merge.sw <<EOF
@prefix : <http://purl.org/rbm/test/> .
@read :tcs  <tcs.ttl>
@read :rbmo <${ROOT}/examples/rbmo.ttl>
@merge (:tcs :rbmo) => :merged
@write :merged ; Hello
EOF

cd ${TMPDIR}

${KRDF} -f ${ROOT}/examples/tcs.kappa -a -n -m > tmp.ttl || error "Error running krdf"
rapper -i turtle -o turtle tmp.ttl > tcs.ttl || error "Error prettifying output of krdf"

(Swish -s=merge.sw || error "Error merging ontology and data") | \
    sed '/^Swish/d' > test.ttl

rapper -i turtle -o rdfxml test.ttl > test.rdf || error "Error converting ttl to rdf"

java -jar ${HERMIT} -v2 -PU test.rdf > unsatisfiable.txt || error "Error running HermiT"

cat unsatisfiable.txt

exit 0

# Local Variables:
# compile-command: "cd ..; cabal build; cabal test"
# End:
