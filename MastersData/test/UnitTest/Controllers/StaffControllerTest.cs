using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace DDDSample1.Tests.UnitTests.Controllers
{
    public class StaffControllerTest
    {
        private Mock<IStaffService>? _mockService;
        private StaffController? _controller;

        [Fact]
        public async Task Create_ReturnsCreatedAtActionResult_WhenStaffIsCreated()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);
            var _staffIdGeneratorService = new StaffIdGeneratorService();


            var staffDto = new StaffDto
            (
                _staffIdGeneratorService.generateStaffId(Category.Doctor, DateTime.Now),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            );
            _mockService.Setup(service => service.AddAsync(staffDto)).ReturnsAsync(staffDto);

            // Act
            var result = await _controller.Create(staffDto);

            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            Assert.Equal(nameof(_controller.GetGetById), actionResult.ActionName);
            Assert.Equal(staffDto, actionResult.Value);
        }

        [Fact]
        public async Task Create_ReturnsBadRequest_WhenExceptionIsThrown()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);
            var _staffIdGeneratorService = new StaffIdGeneratorService();

            // Arrange
            var dto = new StaffDto
            (
                _staffIdGeneratorService.generateStaffId(Category.Doctor, DateTime.Now),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            );
            _mockService.Setup(service => service.AddAsync(dto)).ThrowsAsync(new Exception("Error"));

            // Act
            var result = await _controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.Equal("Error", actionResult.Value?.GetType().GetProperty("Message")?.GetValue(actionResult.Value));
        }

        [Fact]
        public async Task Update_ReturnsOkResult_WhenStaffIsUpdated()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            var dto = new EditingStaffProfileDto("d202412345", "John Doe", "12345", "john.doe@example.com", "+351 1234567890");
            var staffDto = new StaffDto
            (
                new StaffId("d202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            );
            _mockService.Setup(service => service.UpdateAsync(dto)).ReturnsAsync(staffDto);

            // Act
            var result = await _controller.Update(dto, "123");

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            Assert.Equal(staffDto, actionResult.Value);
        }

        [Fact]
        public async Task Update_ReturnsBadRequest_WhenIdDoesNotMatch()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);


            // Arrange
            var dto = new EditingStaffProfileDto("d202412345", "John Doe", "12345", "john.doe@example.com", "+351 1234567890");

            // Act
            var result = await _controller.Update(dto, "456");

            // Assert
            Assert.IsType<BadRequestResult>(result.Result);
        }

        [Fact]
        public async Task Delete_ReturnsOkResult_WhenStaffIsDeleted()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            var staff = new Staff
            (
                new StaffId("d202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            );
            _mockService.Setup(service => service.DeleteAsync(It.IsAny<StaffId>())).ReturnsAsync(staff);

            // Act
            var result = await _controller.Delete("123");

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            Assert.Equal(staff, actionResult.Value);
        }

        [Fact]
        public async Task GetGetById_ReturnsStaff_WhenStaffExists()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            var staff = new Staff
            (
                new StaffId("d202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            );
            _mockService.Setup(service => service.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(staff);

            // Act
            var result = await _controller.GetGetById("123");

            // Assert
            var actionResult = Assert.IsType<ActionResult<Staff>>(result);
            Assert.Equal(staff, actionResult.Value);
        }

        [Fact]
        public async Task GetGetById_ReturnsNotFound_WhenStaffDoesNotExist()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            _mockService.Setup(service => service.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync((Staff?)null);

            // Act
            var result = await _controller.GetGetById("123");

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task GetAll_ReturnsAllStaff()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            var staffList = new List<Staff> {
                new Staff
                (
                new StaffId("d202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            ) ,
                new Staff
                (new StaffId("d202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true"
            )};
            _mockService.Setup(service => service.GetAllAsync()).ReturnsAsync(staffList);

            // Act
            var result = await _controller.GetAll();

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<Staff>>>(result);
            Assert.Equal(staffList, actionResult.Value);
        }

        [Fact]
        public async Task SearchAsync_ReturnsStaff_WhenStaffMatchSearchCriteria()
        {
            _mockService = new Mock<IStaffService>();
            _controller = new StaffController(_mockService.Object);

            // Arrange
            var dto = new StaffFilterDto();
            var staffList = new List<ViewStaffDto>();
            _mockService.Setup(service => service.SearchAsync(dto)).ReturnsAsync(staffList);

            // Act
            var result = await _controller.SearchAsync(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<ViewStaffDto>>>(result);
            Assert.Equal(staffList, actionResult.Value);
        }
    }
}
