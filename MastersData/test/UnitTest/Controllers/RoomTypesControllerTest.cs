using System;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.RoomTypes;
using MastersData.Domain.RoomTypes;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

public class RoomTypesControllerTest
{
    private Mock<IRoomTypeService>? _serviceMock;
    private RoomTypesController? _controller;

    [Fact]

    public async Task AddAsync_ExistingInternalCode_ReturnsBadRequest()
    {

        _serviceMock = new Mock<IRoomTypeService>();
        _controller = new RoomTypesController(_serviceMock.Object);

        // Arrange
        var dto = new AddRoomTypeDto { InternalCode = "EXISTING_CODE", Designation = "Designation", SuitableForSurgeries = true };
        _serviceMock.Setup(service => service.AddRoomTypeAsync(dto))
            .ThrowsAsync(new Exception("Internal code already exists"));

        // Act
        var result = await _controller.AddAsync(dto);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result.Result);
        Assert.NotNull(badRequestResult.Value);
        Assert.Equal("{ Message = Internal code already exists }", badRequestResult.Value.ToString());
    }
}
