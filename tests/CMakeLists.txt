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

#ADD_EXECUTABLE(ssIdCriteriaTest.exe SSIdCriteriaUnitTests.cpp)
#TARGET_LINK_LIBRARIES(ssIdCriteriaTest.exe strideSearch)
#ADD_TEST(NAME ssIdCriteria COMMAND ${PROJECT_BINARY_DIR}/bin/ssIdCriteriaTest.exe)
#
#ADD_EXECUTABLE(ssEventTest.exe SSEventUnitTests.cpp)
#TARGET_LINK_LIBRARIES(ssEventTest.exe strideSearch)
#ADD_TEST(NAME ssEvent COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventTest.exe)

#ADD_EXECUTABLE(ssSectorUnitTest.exe SSSectorUnitTests.cpp)
#TARGET_LINK_LIBRARIES(ssSectorUnitTest.exe strideSearch)
#ADD_TEST(NAME ssSector COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorUnitTest.exe)
#
#ADD_EXECUTABLE(ssEventSetUnitTest.exe SSEventSetUnitTests.cpp)
#TARGET_LINK_LIBRARIES(ssEventSetUnitTest.exe strideSearch)
#ADD_TEST(NAME ssEventSet COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventSetUnitTest.exe)


#ADD_EXECUTABLE(ssWorkspaceDictTest.exe SSWorkspaceDictUnitTest.cpp)
#TARGET_LINK_LIBRARIES(ssWorkspaceDictTest.exe strideSearch)
#ADD_TEST(NAME ssWorkspaceDict COMMAND ${PROJECT_BINARY_DIR}/bin/ssWorkspaceDictTest.exe)

#ADD_EXECUTABLE(ssSectorListUnitTest.exe SSSectorListUnitTest.cpp)
#TARGET_LINK_LIBRARIES(ssSectorListUnitTest.exe strideSearch)
#ADD_TEST(NAME ssSectorList COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorListUnitTest.exe)
#
#ADD_EXECUTABLE(ssDataUnitTest.exe SSDataUnitTest.cpp)
#TARGET_LINK_LIBRARIES(ssDataUnitTest.exe strideSearch)
#ADD_TEST(NAME ssDataUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssDataUnitTest.exe)
#
#ADD_EXECUTABLE(ssDataLatLonUnitTest.exe SSDataLatLonUnitTest.cpp)
#TARGET_LINK_LIBRARIES(ssDataLatLonUnitTest.exe strideSearch)
#ADD_TEST(NAME ssDataLatLonUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssDataLatLonUnitTest.exe)
#
#ADD_EXECUTABLE(ssGridsTest.exe SSGridTests.cpp)
#TARGET_LINK_LIBRARIES(ssGridsTest.exe strideSearch)
#ADD_TEST(NAME ssGridsTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssGridsTest.exe)
#
#ADD_EXECUTABLE(ssTimerUnitTest.exe SSTimerUnitTests.cpp)
#TARGET_LINK_LIBRARIES(ssTimerUnitTest.exe strideSearch)
#ADD_TEST(NAME ssTimerUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssTimerUnitTest.exe)

#ADD_EXECUTABLE(ssWorkspaceTest.exe SSWorkspaceTests.cpp)
#TARGET_LINK_LIBRARIES(ssWorkspaceTest.exe strideSearch)
#ADD_TEST(NAME SSWorkspace COMMAND ${PROJECT_BINARY_DIR}/bin/ssWorkspaceTest.exe)
#
#ADD_EXECUTABLE(ssLLDataTest.exe SSDataLatLonTests.cpp)
#TARGET_LINK_LIBRARIES(ssLLDataTest.exe strideSearch)
#ADD_TEST(NAME SSLLData COMMAND ${PROJECT_BINARY_DIR}/bin/ssLLDataTest.exe)
#
#ADD_EXECUTABLE(ssLLSectorTest.exe SSLLSectorListTests.cpp)
#TARGET_LINK_LIBRARIES(ssLLSectorTest.exe strideSearch)
#ADD_TEST(NAME SSLLSectors COMMAND ${PROJECT_BINARY_DIR}/bin/ssLLSectorTest.exe)

#ADD_EXECUTABLE(ssSectorListTest.exe SSSectorListTests.cpp)
#TARGET_LINK_LIBRARIES(ssSectorListTest.exe strideSearch)
#ADD_TEST(NAME SSSectorList COMMAND ${PROJECT_BINARY_DIR}/ssSectorListTest.exe)
