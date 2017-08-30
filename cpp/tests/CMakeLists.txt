#
#   Testing
#
ADD_EXECUTABLE(ssUtilTest.exe SSUtilitiesTests.cpp)
TARGET_LINK_LIBRARIES(ssUtilTest.exe strideSearch)
ADD_TEST(NAME SSUtil COMMAND ${PROJECT_BINARY_DIR}/bin/ssUtilTest.exe)

ADD_EXECUTABLE(ssEventTest.exe SSEventTests.cpp)
TARGET_LINK_LIBRARIES(ssEventTest.exe strideSearch)
ADD_TEST(NAME SSEvent COMMAND ${PROJECT_BINARY_DIR}/bin/ssEventTest.exe)

ADD_EXECUTABLE(ssDateTimeTest.exe SSDateTimeTests.cpp)
TARGET_LINK_LIBRARIES(ssDateTimeTest.exe strideSearch)
ADD_TEST(NAME SSDatetime COMMAND ${PROJECT_BINARY_DIR}/bin/ssDateTimeTest.exe)

ADD_EXECUTABLE(ssWorkspaceDictTest.exe SSWorkspaceDictUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssWorkspaceDictTest.exe strideSearch)
ADD_TEST(NAME SSWorkspaceDict COMMAND ${PROJECT_BINARY_DIR}/bin/ssWorkspaceDictTest.exe)

ADD_EXECUTABLE(ssMinMaxCriteriaUnitTest.exe SSMinMaxCriteriaUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssMinMaxCriteriaUnitTest.exe strideSearch)
ADD_TEST(NAME SSMinMaxCriteria COMMAND ${PROJECT_BINARY_DIR}/bin/ssMinMaxCriteriaUnitTest.exe)

ADD_EXECUTABLE(ssSectorUnitTest.exe SSSectorUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssSectorUnitTest.exe strideSearch)
ADD_TEST(NAME SSSector COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorUnitTest.exe)

ADD_EXECUTABLE(ssSectorListUnitTest.exe SSSectorListUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssSectorListUnitTest.exe strideSearch)
ADD_TEST(NAME SSSectorList COMMAND ${PROJECT_BINARY_DIR}/bin/ssSectorListUnitTest.exe)

ADD_EXECUTABLE(ssDataLatLonUnitTest.exe SSDataLatLonUnitTest.cpp)
TARGET_LINK_LIBRARIES(ssDataLatLonUnitTest.exe strideSearch)
ADD_TEST(NAME ssDataLatLonUnitTest COMMAND ${PROJECT_BINARY_DIR}/bin/ssDataLatLonUnitTest.exe)


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