using Xunit;
using Moq;
using DDDSample1.Controllers;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.PatientLoggers;

public class UsersControllerUnitTests
{
    private Mock<UserService>? _mockUserService;
    private UsersController? _controller;

    [Fact]
    public async Task RegisterPatientAsync_ValidCreate_ReturnsOk()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new RegisteringPatientDto {Name = "John Doe", Email = "john.doe@gmail.com", PhoneNumber = "+351 123456789", Street = "Main Street", PostalCode = "1234-567", City = "Los Angeles", Country = "USA", Password = "123456789"};
        var confirmationDto = new ConfirmationPatientDto("token", "john.doe@gmail.com");

        _mockUserService.Setup(service => service.RegisterPatientAsync(It.IsAny<RegisteringPatientDto>()))
                        .ReturnsAsync(confirmationDto);

        // Act
        var result = await _controller.RegisterPatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<OkObjectResult>(result);
        var returnValue = Assert.IsType<ConfirmationPatientDto>(actionResult.Value);
        Assert.Equal(confirmationDto, returnValue);
    }

    [Fact]
    public async Task RegisterPatientAsync_InvalidCreate_ReturnsBadRequest()
    {
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new RegisteringPatientDto {Name = "John Doe", Email = "john.doe@gmail.com", PhoneNumber = "+351 123456789", Street = "Main Street", PostalCode = "1234-567", City = "Los Angeles", Country = "USA", Password = "123456789"};

        _mockUserService.Setup(service => service.RegisterPatientAsync(It.IsAny<RegisteringPatientDto>()))
                        .ThrowsAsync(new Exception("Invalid data"));
        
        // Act
        var result = await _controller.RegisterPatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.IsType<NotFoundResult>(actionResult);
    }

    [Fact]
    public async Task ConfirmRegisterPatientAsync_ValidConfirm_ReturnsOk()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new ConfirmationPatientDto("token", "john.doe@gmail.com");
        var patientDto = new PatientDto(Guid.NewGuid(), "John Doe", "2024-10-10", "male", "john.doe@gmail.com", "+351 123456789", "202410000001",
            new AddressDto("Main Street", "1234-567", "Los Angeles", "USA"), 
            new EmergencyContactDto("Jane Doe", "jane.doe@gmail.com", "+351 987654321"), null);

        _mockUserService.Setup(service => service.ConfirmRegisterPatientAsync(It.IsAny<ConfirmationPatientDto>()))
                        .ReturnsAsync(patientDto);
        
        // Act
        var result = await _controller.ConfirmRegisterPatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ConfirmRegisterPatientAsync_InvalidConfirm_ReturnsNotFound()
    {
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new ConfirmationPatientDto("token", "john.doe@gmail.com");

        _mockUserService.Setup(service => service.ConfirmRegisterPatientAsync(It.IsAny<ConfirmationPatientDto>()))
                        .ThrowsAsync(new BusinessRuleValidationException("Invalid token"));

        // Act
        var result = await _controller.ConfirmRegisterPatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<NotFoundResult>(result);
    }


    [Fact]
    public async Task EditPatientAsync_ValidEdit_ReturnsOk()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new EditingPatientDto { Email = "john.doe@gmail.com", EmailToEdit = "john.doing@gmail.com"};
        var confirmationDto = new ConfirmationEditPatientDto("token", "john.doe@gmail.com", "john.doing@gmail.com", "123456789");

        _mockUserService.Setup(service => service.EditPatientAsync(It.IsAny<EditingPatientDto>()))
                        .ReturnsAsync(confirmationDto);

        // Act
        var result = await _controller.EditPatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<OkObjectResult>(result);
        var returnValue = Assert.IsType<ConfirmationEditPatientDto>(actionResult.Value);
        Assert.Equal(confirmationDto, returnValue);
    }

    [Fact]
    public async Task EditPatientAsync_InvalidEdit_ReturnsBadRequest()
    {
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new EditingPatientDto {Email = "error@example.com"} ; // Invalid DTO

        _mockUserService.Setup(service => service.EditPatientAsync(It.IsAny<EditingPatientDto>()))
                        .ThrowsAsync(new Exception("Invalid data"));

        // Act
        var result = await _controller.EditPatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.NotNull(actionResult);
    }

    [Fact]
    public async Task ConfirmEditPatientAsync_ValidConfirm_ReturnsOk()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new ConfirmationEditPatientDto("token", "john.doe@example.com", "john.doing@example.com", "123456789");
        var patientDto = new PatientDto(Guid.NewGuid(), "John Doe", "2024-10-10", "male", "john.doe@gmail.com", "+351 123456789", "202410000001",
            new AddressDto("Main Street", "1234-567", "Los Angeles", "USA"), 
            new EmergencyContactDto("Jane Doe", "jane.doe@gmail.com", "+351 987654321"), null);

        _mockUserService.Setup(service => service.ConfirmEditPatientAsync(It.IsAny<ConfirmationEditPatientDto>()))
                        .ReturnsAsync(patientDto);
        
        // Act
        var result = await _controller.ConfirmEditPatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<OkObjectResult>(result);
        var returnValue = Assert.IsType<PatientDto>(actionResult.Value);
        Assert.Equal(patientDto, returnValue);
    }

    [Fact]
    public async Task ConfirmEditPatientAsync_InvalidConfirm_ReturnsNotFound()
    {
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new ConfirmationEditPatientDto("token", "john.doe@example.com", "john.doing@example.com", "123456789");

        _mockUserService.Setup(service => service.ConfirmEditPatientAsync(It.IsAny<ConfirmationEditPatientDto>()))
                        .ThrowsAsync(new BusinessRuleValidationException("Invalid token"));

        // Act
        var result = await _controller.ConfirmEditPatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<NotFoundResult>(result);
        Assert.NotNull(actionResult);
    }

    [Fact]
    public async Task DeletePatientAsync_ValidDelete_ReturnsOk()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new DeletingPatientDto { Email = "john.doe@example.com"};
        var confirmationDto = new ConfirmationPatientDto("token", "john.doe@example.com");

        _mockUserService.Setup(service => service.DeletePatientAsync(It.IsAny<DeletingPatientDto>()))
                        .ReturnsAsync(confirmationDto);
        
        // Act
        var result = await _controller.DeletePatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task DeletePatientAsync_InvalidDelete_ReturnsBadRequest()
    {
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new DeletingPatientDto { Email = "john.doe@example.com"};
        
        _mockUserService.Setup(service => service.DeletePatientAsync(It.IsAny<DeletingPatientDto>()))
                        .ThrowsAsync(new Exception("Invalid data"));

        // Act
        var result = await _controller.DeletePatientAsync(dto);

        // Assert
        var actionResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.NotNull(actionResult);
    }

    [Fact]
    public async Task ConfirmDeletePatientAsync_ValidConfirm_ReturnsNoContent()
    {
        // Arrange
        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);

        var dto = new ConfirmationPatientDto("token", "john.doe@gmail.com");

        _mockUserService.Setup(service => service.ConfirmDeletePatientAsync(It.IsAny<ConfirmationPatientDto>()));

        // Act
        var result = await _controller.ConfirmDeletePatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<NoContentResult>(result);
    }

    [Fact]
    public async Task ConfirmDeletePatientAsync_InvalidConfirm_ReturnsNotFound()
    {   

        _mockUserService = new Mock<UserService>(MockBehavior.Loose);
        _controller = new UsersController(_mockUserService.Object);
        // Arrange
        var dto = new ConfirmationPatientDto("token", "john.doe@gmail.com");

        _mockUserService.Setup(service => service.ConfirmDeletePatientAsync(It.IsAny<ConfirmationPatientDto>()))
                        .ThrowsAsync(new BusinessRuleValidationException("Invalid token"));
        
        // Act
        var result = await _controller.ConfirmDeletePatientAsync("token", dto);

        // Assert
        var actionResult = Assert.IsType<NotFoundResult>(result);
        Assert.NotNull(actionResult);
    }

    
}
