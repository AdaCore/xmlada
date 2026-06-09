
echo "=================================="
echo "== Entity Expansion Count Limit =="
echo "=================================="
${XMLADA_ROOT}/tests/sax/obj/testsax bomb.xml --max-expansion-count=10 -nocolor

echo "=================================="
echo "== Entity Expansion Size Limit =="
echo "=================================="
${XMLADA_ROOT}/tests/sax/obj/testsax bomb.xml --max-expansion-size=16 -nocolor
