using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace DDDSample1.Tests.UnitTests.Controllers
{
    public class OperationTypeControllerTest
    {
        private Mock<IOperationTypeService>? _mockService;
        private Mock<ISpecializationService>? _mockSpecializationService;
        private OperationTypeController? _controller;

        [Fact]
        public async Task Create_ReturnsBadRequest_WhenDtoIsNull()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Act
            var result = await _controller.Create(null);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task Create_ReturnsCreatedResult_WhenDtoIsValid()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                "Test Operation Type",
                "active",
                new PhaseDTO(
                    Guid.NewGuid(),
                    20,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", Guid.NewGuid().ToString()),
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    90,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    15,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", Guid.NewGuid().ToString())
                    }
                ),
                Guid.NewGuid().ToString()
            );

            var operationType = OperationTypeMapper.toDomain(dto);
            _mockService.Setup(s => s.CreateAsync(It.IsAny<OperationType>())).ReturnsAsync(operationType);
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(okResult.Value);
            Assert.Equal(dto.Name, returnValue.Name);
        }

        [Fact]
        public async Task GetById_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Arrange
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _controller.GetById(Guid.NewGuid().ToString());

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        
        
        

        [Fact]
        public async Task Inactivate_ReturnsOk_WhenOperationTypeIsDeactivated()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Arrange
            var operationType = new OperationType(
                        "Test Operation Type", 
                        true, 
                        new Phase(20, new List<RequiredStaff> { new RequiredStaff(10, new SpecializationId(Guid.NewGuid().ToString())), new RequiredStaff(20, new SpecializationId(Guid.NewGuid().ToString())) }),
                        new Phase(90, new List<RequiredStaff> { new RequiredStaff(20, new SpecializationId(Guid.NewGuid().ToString())) }),
                        new Phase(15, new List<RequiredStaff> { new RequiredStaff(2, new SpecializationId(Guid.NewGuid().ToString())) }),
                        new SpecializationId(Guid.NewGuid().ToString())
                    );
            _mockService.Setup(s => s.Deactivate(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _controller.Inactivate(Guid.NewGuid().ToString());

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(okResult.Value);
            Assert.Equal("Test Operation Type", returnValue.Name);
        }

        
        [Fact]
        public async Task Update_ReturnsBadRequest_WhenIdDoesNotMatchDtoId()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                "Test Operation Type",
                "active",
                new PhaseDTO(
                    Guid.NewGuid(),
                    20,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", Guid.NewGuid().ToString()),
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    90,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    15,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", Guid.NewGuid().ToString())
                    }
                ),
                Guid.NewGuid().ToString()
            );

            // Act
            var result = await _controller.Update(Guid.NewGuid(), dto);

            // Assert
            Assert.IsType<BadRequestResult>(result.Result);
        }

        [Fact]
        public async Task Update_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockSpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockSpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                "Test Operation Type",
                "active",
                new PhaseDTO(
                    Guid.NewGuid(),
                    20,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", Guid.NewGuid().ToString()),
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    90,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                ),
                new PhaseDTO(
                    Guid.NewGuid(),
                    15,
                    new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", Guid.NewGuid().ToString())
                    }
                ),
                Guid.NewGuid().ToString()
            );
            _mockService.Setup(s => s.UpdateAsync(dto)).ReturnsAsync((OperationType)null);

            // Act
            var result = await _controller.Update(dto.Id, dto);

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

    }
}