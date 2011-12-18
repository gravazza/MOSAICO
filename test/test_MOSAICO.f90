PROGRAM test_MOSAICO

    ! Modules used:
    USE LogLib, ONLY: &
    !Imported routines:
    LogInit, LogStop

    USE DataTypeSizes, ONLY: &
    !Imported parameters:
    short, float

    USE GeoLib, ONLY: &
    !Imported routines:
    GeoInit, SetCRS, SetUTMparameters, &
    !Imported parameters:
    UTM, ED50, NORTH, &
    !Imported operators:
    ASSIGNMENT (=)

    USE GridLib, ONLY: &
    !Imported routines:
    NewGrid, &
    !Imported definitions:
    grid_integer, grid_real, &
    !Imported parameters:
    ESRI_ASCII, NET_CDF

    USE GridOperations, ONLY: &
    !imported routines:
    GridConvert, GridResample, GetMean

    USE Chronos, ONLY: &
    !Imported definitions:
    DateTime, &
    !Imported variables:
    timeString, &
    !Imported operators:
    ASSIGNMENT (=), OPERATOR (+)

    USE Utilities, ONLY: &
    !Imported routines:
    GetUnit

    USE Units, ONLY: &
    !Imported parameters:
    month


    IMPLICIT NONE

    TYPE (grid_integer)    :: mask
    TYPE (grid_real)       :: precipitation
    TYPE (grid_real)       :: precipitation_po
    TYPE (grid_real)       :: temporary_grid
    INTEGER (KIND = short) :: i
    TYPE (DateTime)        :: time
    INTEGER (KIND = short) :: fileUnit
    INTEGER (KIND = short) :: dt
    REAL (KIND = float)    :: mean

    !------------end of declaration---------------------------------------------


    !------------------------------------------
    !    initialize log
    !------------------------------------------

    CALL LogInit ()


    !------------------------------------------
    !    initialize cartographic engine
    !------------------------------------------

    CALL GeoInit ('GeoLib.ini')


    !-------------------------------------------------
    !    read mask and set coordinate reference system
    !-------------------------------------------------

    !read grid
    CALL NewGrid (layer = mask, fileName = 'po_mask.asc', fileFormat = ESRI_ASCII)

    !set CRS
    CALL SetCRS (CRStype = UTM, datumType = ED50, rs = mask % grid_mapping)

    !set UTM parameters
    CALL SetUTMparameters (system = mask % grid_mapping, zone = 32, &
    hemisphere = NORTH, override = 1)


    !---------------------------------------------------
    !    read time variant temperature in netCDF dataset
    !---------------------------------------------------
    !initialize time counter
    timeString = '1800-01-01T00:00:00+00:00'
    time =  timeString
    dt = month
    !open file for output
    fileUnit = GetUnit ()
    OPEN (UNIT = fileUnit,file = 'output.txt')
    !initialize Po precipitation map using mask as template
    CALL NewGrid (layer = precipitation_po, grid = mask)
    !Assign coordinate reference system to temporary grid
    temporary_grid % grid_mapping = mask % grid_mapping
    !loop
    DO i = 1, 2460

        !read precipitation from netcdf
        CALL NewGrid (layer = precipitation, fileName = 'alp_pre_dat.nc' ,  &
        fileFormat = NET_CDF, variable = 'precipitation', time = time)

        !coordinate conversion
        CALL GridConvert (GridIn = precipitation, GridOut = temporary_grid, &
        cellsize = precipitation_po % cellsize)
        CALL GridResample (grid = temporary_grid, resampledGrid = precipitation_po)

        !compute mean precipitation over Po river basin
        mean = GetMean (precipitation_po)

        !write result
        WRITE (*,*) i, mean
        WRITE (fileUnit,*) i, mean
        time = time + dt
    END DO
    !close output file
    CLOSE (fileUnit)

    !------------------------------------------
    !    terminate log
    !------------------------------------------

    CALL LogStop ()


END PROGRAM test_MOSAICO
