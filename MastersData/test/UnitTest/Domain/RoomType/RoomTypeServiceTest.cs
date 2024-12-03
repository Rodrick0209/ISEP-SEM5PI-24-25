using System;
using System.Threading.Tasks;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Domain.Shared;
using MastersData.Domain.RoomTypes;
using Moq;
using Xunit;

public class RoomTypeServiceTest
{
    private Mock<IUnitOfWork>? _unitOfWorkMock;
    private Mock<IRoomTypeRepository>? _roomTypeRepositoryMock;
    private RoomTypeService? _roomTypeService;

    [Fact]
    public async Task AddRoomTypeAsync_WithExistingInternalCode_ShouldThrowBusinessRuleValidationException()
    {

        _unitOfWorkMock = new Mock<IUnitOfWork>();
        _roomTypeRepositoryMock = new Mock<IRoomTypeRepository>();
        _roomTypeService = new RoomTypeService(_unitOfWorkMock.Object, _roomTypeRepositoryMock.Object);

        // Arrange
        var existingInternalCode = "2024-001";
        var addRoomTypeDto = new AddRoomTypeDto
        {
            InternalCode = existingInternalCode,
            Designation = "Test Designation",
            Description = "Test Description",
            SuitableForSurgeries = true
        };

        _roomTypeRepositoryMock.Setup(repo => repo.GetByInternalCodeAsync(existingInternalCode))
            .ReturnsAsync(new RoomType(existingInternalCode, "Existing Designation", "Existing Description", true));

        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _roomTypeService.AddRoomTypeAsync(addRoomTypeDto));
    }
}