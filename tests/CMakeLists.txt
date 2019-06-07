#
#   Testing
#
INCLUDE(CTest)

ADD_EXECUTABLE(ssDefsTest.exe SSDefsUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssDefsTest.exe strideSearch)
ADD_TEST(NAME ssDefs COMMAND ${PROJECT_BINARY_DIR}/bin/ssDefsTest.exe)

ADD_EXECUTABLE(ssUtilTest.exe SSUtilitiesTests.cpp)
TARGET_LINK_LIBRARIES(ssUtilTest.exe strideSearch)
ADD_TEST(NAME ssUtil COMMAND ${PROJECT_BINARY_DIR}/bin/ssUtilTest.exe)

ADD_EXECUTABLE(ssNcReaderTest.exe SSNCReaderUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssNcReaderTest.exe strideSearch)
ADD_TEST(NAME ssNcReader COMMAND ${PROJECT_BINARY_DIR}/bin/ssNcReaderTest.exe)

ADD_EXECUTABLE(ssDateTimeTest.exe SSDateTimeTests.cpp)
TARGET_LINK_LIBRARIES(ssDateTimeTest.exe strideSearch)
ADD_TEST(NAME ssDatetime COMMAND ${PROJECT_BINARY_DIR}/bin/ssDateTimeTest.exe)

ADD_EXECUTABLE(ssKdTreeTest.exe SSKdTreeUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssKdTreeTest.exe strideSearch)
ADD_TEST(NAME ssKdTree COMMAND ${PROJECT_BINARY_DIR}/bin/ssKdTreeTest.exe)

ADD_EXECUTABLE(ssWorkspaceTest.exe SSWorkspaceUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssWorkspaceTest.exe strideSearch)
ADD_TEST(NAME ssWorkspace COMMAND ${PROJECT_BINARY_DIR}/bin/ssWorkspaceTest.exe)

ADD_EXECUTABLE(ssIdCriteriaTest.exe SSIdCriteriaUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssIdCriteriaTest.exe strideSearch)
ADD_TEST(NAME ssIdCriteria COMMAND ${PROJECT_BINARY_DIR}/bin/ssIdCriteriaTest.exe)

ADD_EXECUTABLE(ssEventTest.exe SSEventUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssEventTest.exe strideSearch)
ADD_TEST(NAME ssEvent COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventTest.exe)

ADD_EXECUTABLE(ssSectorUnitTest.exe SSSectorUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssSectorUnitTest.exe strideSearch)
ADD_TEST(NAME ssSector COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorUnitTest.exe)

ADD_EXECUTABLE(ssEventSetUnitTest.exe SSEventSetUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssEventSetUnitTest.exe strideSearch)
ADD_TEST(NAME ssEventSet COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventSetUnitTest.exe)

ADD_EXECUTABLE(ssSectorSetUnitTest.exe SSSectorSetUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssSectorSetUnitTest.exe strideSearch)
ADD_TEST(NAME ssSectorSet COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorSetUnitTest.exe)

ADD_EXECUTABLE(ssSearchManagerUnitTest.exe SSSearchManagerUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssSearchManagerUnitTest.exe strideSearch)
ADD_TEST(NAME ssSearchManager COMMAND ${PROJECT_BINARY_DIR}/bin/ssSearchManagerUnitTest.exe)

ADD_EXECUTABLE(ssTropicalCycloneTest.exe SSTropicalCycloneTest.cpp)
TARGET_LINK_LIBRARIES(ssTropicalCycloneTest.exe strideSearch)
ADD_TEST(NAME ssTropicalCyclone COMMAND ${PROJECT_BINARY_DIR}/bin/ssTropicalCycloneTest.exe)

ADD_EXECUTABLE(ssMPIMangagerUnitTest.exe SSMPIManagerUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssMPIMangagerUnitTest.exe strideSearch)
ADD_TEST(NAME ssMPIMangager COMMAND ${PROJECT_BINARY_DIR}/bin/ssMPIMangagerUnitTest.exe)

ADD_EXECUTABLE(ssInputUnitTest.exe SSInputUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssInputUnitTest.exe strideSearch)
ADD_TEST(NAME ssInput COMMAND ${PROJECT_BINARY_DIR}/bin/ssInputUnitTest.exe)

ADD_EXECUTABLE(ssAvgAreaTest.exe SSAverageAreaTest.cpp)
TARGET_LINK_LIBRARIES(ssAvgAreaTest.exe strideSearch)
ADD_TEST(NAME ssAvgArea COMMAND ${PROJECT_BINARY_DIR}/bin/ssAvgAreaTest.exe)

ADD_EXECUTABLE(ssTrackTest.exe SSTrackTest.cpp)
TARGET_LINK_LIBRARIES(ssTrackTest.exe strideSearch)
ADD_TEST(NAME ssTrack COMMAND ${PROJECT_BINARY_DIR}/bin/ssTrackTest.exe)



