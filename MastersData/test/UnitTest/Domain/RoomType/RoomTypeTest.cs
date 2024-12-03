using System;
using Xunit;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Domain.Shared;

public class RoomTypeTest
{
    [Fact]
    public void Test1_VerifyNewRoomTypeCanBeAddedWithValidData()
    {
        // Arrange
        var internalCode = "3000-001";
        var designation = "General Room";
        var description = "A general room for patients.";
        var suitableForSurgeries = true;

        // Act
        var roomType = new RoomType(internalCode, designation, description, suitableForSurgeries);

        // Assert
        Assert.NotNull(roomType);
        Assert.Equal(internalCode, roomType.InternalCode.internalCode);
        Assert.Equal(designation, roomType.Designation.fullName);
        Assert.Equal(description, roomType.Description.description);
        Assert.Equal(suitableForSurgeries, roomType.SuitableForSurgeries.suitableForSurgeries);
    }

    [Fact]
    public void Test3_VerifyAddingRoomTypeWithInvalidInternalCodeFails()
    {
        // Arrange
        var internalCode = "ABC 1234"; // Invalid internal code
        var designation = "General Room";
        var description = "A general room for patients.";
        var suitableForSurgeries = true;

        // Act & Assert
        Assert.Throws<BusinessRuleValidationException>(() => new RoomType(internalCode, designation, description, suitableForSurgeries));
    }

    [Fact]
    public void Test4_VerifyAddingRoomTypeWithDesignationLongerThan100CharactersFails()
    {
        // Arrange
        var internalCode = "3000-001";
        var designation = new string('a', 101); // Designation longer than 100 characters
        var description = "A general room for patients.";
        var sultabilityForSurgeries = true;

        // Act & Assert
        Assert.Throws<BusinessRuleValidationException>(() => new RoomType(internalCode, designation, description, sultabilityForSurgeries));
    }

    [Fact]
    public void Test5_VerifyAddingRoomTypeWithoutDescriptionSucceeds()
    {
        // Arrange
        var internalCode = "3000-001";
        var designation = "General Room";
        var description = ""; // No description
        var sultabilityForSurgeries = true;

        // Act
        var roomType = new RoomType(internalCode, designation, description, sultabilityForSurgeries);

        // Assert
        Assert.NotNull(roomType);
        Assert.Equal(internalCode, roomType.InternalCode.internalCode);
        Assert.Equal(designation, roomType.Designation.fullName);
        Assert.Equal(description, roomType.Description?.description);
        Assert.Equal(sultabilityForSurgeries, roomType.SuitableForSurgeries.suitableForSurgeries);
    }

    [Fact]
    public void Test6_VerifyAddingRoomTypeWithSuitabilityForSurgeriesSetToTrueOrFalseSucceeds()
    {
        // Arrange
        var internalCode1 = "3000-001";
        var designation1 = "General Room";
        var description1 = "A general room for patients.";
        var suitableForSurgeries1 = true;

        var internalCode2 = "3000-002";
        var designation2 = "Surgery Room";
        var description2 = "A room suitable for surgeries.";
        var suitableForSurgeries2 = false;

        // Act
        var roomType1 = new RoomType(internalCode1, designation1, description1, suitableForSurgeries1);
        var roomType2 = new RoomType(internalCode2, designation2, description2, suitableForSurgeries2);

        // Assert
        Assert.NotNull(roomType1);
        Assert.Equal(suitableForSurgeries1, roomType1.SuitableForSurgeries.suitableForSurgeries);

        Assert.NotNull(roomType2);
        Assert.Equal(suitableForSurgeries2, roomType2.SuitableForSurgeries.suitableForSurgeries);
    }
}
