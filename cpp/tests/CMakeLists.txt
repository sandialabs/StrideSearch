#
#   Testing
#
INCLUDE(CTest)

ADD_EXECUTABLE(ssUtilTest.exe SSUtilitiesTests.cpp)
TARGET_LINK_LIBRARIES(ssUtilTest.exe strideSearch)
ADD_TEST(NAME ssUtil COMMAND ${PROJECT_BINARY_DIR}/bin/ssUtilTest.exe)

ADD_EXECUTABLE(ssDateTimeTest.exe SSDateTimeTests.cpp)
TARGET_LINK_LIBRARIES(ssDateTimeTest.exe strideSearch)
ADD_TEST(NAME ssDatetime COMMAND ${PROJECT_BINARY_DIR}/bin/ssDateTimeTest.exe)

ADD_EXECUTABLE(ssTimerUnitTest.exe SSTimerUnitTests.cpp)
TARGET_LINK_LIBRARIES(ssTimerUnitTest.exe strideSearch)
ADD_TEST(NAME ssTimerUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssTimerUnitTest.exe)

ADD_EXECUTABLE(ssSectorUnitTest.exe SSSectorUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssSectorUnitTest.exe strideSearch)
ADD_TEST(NAME ssSector COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorUnitTest.exe)

#ADD_EXECUTABLE(ssNanoTest.exe SSNanoTest.cpp)
#TARGET_LINK_LIBRARIES(ssNanoTest.exe strideSearch)
#ADD_TEST(NAME ssNano COMMAND ${PROJECT_BINARY_DIR}/bin/ssNanoTest.exe)

ADD_EXECUTABLE(ssEventTest.exe SSEventTests.cpp)
TARGET_LINK_LIBRARIES(ssEventTest.exe strideSearch)
ADD_TEST(NAME ssEvent COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventTest.exe)

ADD_EXECUTABLE(ssWorkspaceDictTest.exe SSWorkspaceDictUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssWorkspaceDictTest.exe strideSearch)
ADD_TEST(NAME ssWorkspaceDict COMMAND ${PROJECT_BINARY_DIR}/bin/ssWorkspaceDictTest.exe)

ADD_EXECUTABLE(ssMinMaxCriteriaUnitTest.exe SSMinMaxCriteriaUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssMinMaxCriteriaUnitTest.exe strideSearch)
ADD_TEST(NAME ssMinMaxCriteria COMMAND ${PROJECT_BINARY_DIR}/bin/ssMinMaxCriteriaUnitTest.exe)



ADD_EXECUTABLE(ssSectorListUnitTest.exe SSSectorListUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssSectorListUnitTest.exe strideSearch)
ADD_TEST(NAME ssSectorList COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorListUnitTest.exe)

ADD_EXECUTABLE(ssDataUnitTest.exe SSDataUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssDataUnitTest.exe strideSearch)
ADD_TEST(NAME ssDataUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssDataUnitTest.exe)

#ADD_EXECUTABLE(ssDataLatLonUnitTest.exe SSDataLatLonUnitTest.cpp)
#TARGET_LINK_LIBRARIES(ssDataLatLonUnitTest.exe strideSearch)
#ADD_TEST(NAME ssDataLatLonUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssDataLatLonUnitTest.exe)

ADD_EXECUTABLE(ssGridsTest.exe SSGridTests.cpp)
TARGET_LINK_LIBRARIES(ssGridsTest.exe strideSearch)
ADD_TEST(NAME ssGridsTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssGridsTest.exe)



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
