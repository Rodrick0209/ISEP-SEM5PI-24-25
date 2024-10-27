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
        private Mock<ISpecializationService>? _mockISpecializationService;
        private OperationTypeController? _controller;

        [Fact]
        public async Task Create_ReturnsBadRequest_WhenDtoIsNull()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Act
            var result = await _controller.Create(null);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task Create_ReturnsCreatedResult_WhenDtoIsValid()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                 "Test Operation Type",
                 "active",
                 new PhaseDTO(),
                 new PhaseDTO(),
                 new PhaseDTO(),
                 Guid.NewGuid().ToString()
            );

            var operationType = new OperationType(dto.Name, dto.Status == "active", null, null, null, new SpecializationId(dto.Specialization));
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
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _controller.GetById(Guid.NewGuid().ToString());

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task GetById_ReturnsOk_WhenOperationTypeExists()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var id = Guid.NewGuid().ToString();
            var operationType = new OperationType("Test Operation Type", true, null, null, null, new SpecializationId(Guid.NewGuid().ToString()));
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _controller.GetById(id);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(okResult.Value);
            Assert.Equal("Test Operation Type", returnValue.Name);
        }

        [Fact]
        public async Task Inactivate_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            _mockService.Setup(s => s.Deactivate(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _controller.Inactivate(Guid.NewGuid().ToString());

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task Inactivate_ReturnsBadRequest_WhenBusinessRuleValidationExceptionIsThrown()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            _mockService.Setup(s => s.Deactivate(It.IsAny<OperationTypeId>())).ThrowsAsync(new BusinessRuleValidationException("Error"));

            // Act
            var result = await _controller.Inactivate(Guid.NewGuid().ToString());

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.Equal("Error", ((dynamic)badRequestResult.Value).Message);
        }

        [Fact]
        public async Task Inactivate_ReturnsOk_WhenOperationTypeIsDeactivated()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var operationType = new OperationType("Test Operation Type", true, null, null, null, new SpecializationId(Guid.NewGuid().ToString()));
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
        public async Task GetAll_ReturnsAllOperationTypes()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var operationTypes = new List<OperationType> { new OperationType("Test Operation Type 1", true, null, null, null, new SpecializationId(Guid.NewGuid().ToString())), new OperationType("Test Operation Type 2", true, null, null, null, new SpecializationId(Guid.NewGuid().ToString())) };
            _mockService.Setup(service => service.GetAllAsync()).ReturnsAsync(operationTypes);

            // Act
            var result = await _controller.GetAll();

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<OperationTypeDto>>>(result);
            Assert.Equal(operationTypes.Count, ((List<OperationTypeDto>)actionResult.Value).Count);
        }

        [Fact]
        public async Task Update_ReturnsBadRequest_WhenIdDoesNotMatchDtoId()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                 "Test Operation Type",
                 "active",
                 new PhaseDTO(),
                 new PhaseDTO(),
                 new PhaseDTO(),
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
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                 "Test Operation Type",
                 "active",
                 new PhaseDTO(),
                 new PhaseDTO(),
                 new PhaseDTO(),
                 Guid.NewGuid().ToString()
            );
            _mockService.Setup(s => s.UpdateAsync(dto)).ReturnsAsync((OperationType)null);

            // Act
            var result = await _controller.Update(dto.Id, dto);

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task Update_ReturnsOk_WhenOperationTypeIsUpdated()
        {
            _mockService = new Mock<IOperationTypeService>();
            _mockISpecializationService = new Mock<ISpecializationService>();
            _controller = new OperationTypeController(_mockService.Object, _mockISpecializationService.Object);

            // Arrange
            var dto = new OperationTypeDto
            (
                Guid.NewGuid(),
                 "Test Operation Type",
                 "active",
                 new PhaseDTO(),
                 new PhaseDTO(),
                 new PhaseDTO(),
                 Guid.NewGuid().ToString()
            );
            var operationType = new OperationType(dto.Name, dto.Status == "active", null, null, null, new SpecializationId(dto.Specialization));
            _mockService.Setup(s => s.UpdateAsync(dto)).ReturnsAsync(operationType);
            _mockService.Setup(s => s.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _controller.Update(dto.Id, dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(okResult.Value);
            Assert.Equal(dto.Name, returnValue.Name);
        }
    }
}