using System;
using System.Collections.Generic;
using System.Security.Claims;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace DDDSample1.Tests.UnitTests.Controllers
{
    public class StaffControllerTest
    {

    private readonly Mock<IOperationRequestService> _mockService;
    private readonly OperationRequestController _controller;
        
        public StaffControllerTest()
        {
            _mockService = new Mock<IOperationRequestService>();
            _controller = new OperationRequestController(_mockService.Object);
        
            var user = new ClaimsPrincipal(new ClaimsIdentity(new Claim[]
            {
            new Claim(ClaimTypes.Email, "D202512345@gmail.com"),
            }, "mock"));

            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext { User = user }
            };        
        }

        [Fact]
        public async Task Create_ValidOperationRequest_ReturnsCreatedOperationRequest()
        {
            // Arrange
            var operationTypeId = Guid.Parse("11111111-1111-1111-1111-111111111111");
            var patientId = Guid.Parse("22222222-2222-2222-2222-222222222222");
            var staffId = "D202512345";

            var doctorEmail = "D202512345@gmail.com";
            var doctorId = new Email(doctorEmail).getFirstPartOfEmail();

            var operationRequest = new OperationRequest(
                "2025-01-01",
                "eletric",
                patientId.ToString(),
                operationTypeId.ToString(),
                staffId,
                staffId
            );

           

            var operationRequestDto = new OperationRequestDto
            (
                "2025-01-01",
                "eletric",
                patientId.ToString(),
                operationTypeId.ToString(),
                doctorId
            );

            _mockService.Setup(service => service.AddAsync(It.IsAny<OperationRequest>()))
                .ReturnsAsync(operationRequest);

            // Act
            var result = await _controller.Create(operationRequestDto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(createdAtActionResult.Value);

            Assert.Equal(nameof(_controller.GetGetById), createdAtActionResult.ActionName);
            Assert.Equal(operationRequestDto.OperationTypeId, returnValue.OperationTypeId);
            Assert.Equal(operationRequestDto.PatientId, returnValue.PatientId);
            Assert.Equal(operationRequestDto.DoctorThatWillPerformId, returnValue.DoctorThatWillPerformId);
        }


        [Fact]
        public async Task PutAsync_WithValidDto_ReturnsSuccess()
        {
            // Arrange
            var doctorEmail = "D202512345@gmail.com";
            var doctorId = new Email(doctorEmail).getFirstPartOfEmail();


            var operationRequest = new OperationRequest(
                "2029-01-01",
                "eletric",
                "patientId",
                "operationTypeId",
                doctorId,
                "doctorThatWillPerformId"
            );




            var changeOperationRequestDto = new ChangeOperationRequestDto(
                operationRequest.Id.AsGuid(),
                "2029-01-01",
                "eletric"
            );

            _mockService.Setup(service => service.UpdateAsync(changeOperationRequestDto,doctorEmail)).ReturnsAsync(operationRequest);

            // Act
            var result = await _controller.Update(operationRequest.Id.AsGuid(), changeOperationRequestDto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(okResult.Value);

            Assert.Equal(changeOperationRequestDto.Id, returnValue.Id);
            Assert.Equal(changeOperationRequestDto.DeadLineDate, returnValue.DeadLineDate);
            Assert.Equal(changeOperationRequestDto.Priority, returnValue.Priority);
        
        
        
        }




        [Fact]
        public async Task DeleteAsync_WithValid_ReturnSuccess()
         {
            // Arrange
            var operationRequest = new OperationRequest(
                    "2025-01-01",
                    "eletric",
                    "patientId",
                    "operationTypeId",
                    "doctorThatRequestedId",
                    "doctorThatWillPerformId"
            );

            _mockService.Setup(service => service.DeleteAsync(operationRequest.Id)).ReturnsAsync(operationRequest);

            // Act
            var result = await _controller.Delete(operationRequest.Id.AsGuid());

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(okResult.Value);

            Assert.Equal(operationRequest.Id.AsGuid(), returnValue.Id);

        }


         [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByMedicalRecordNumber_ReturnsRequests()
        {
            // Arrange
            var filters = new OperationRequestFilterDto { MedicalRecordNumber = "202410000001" };
            var patient = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");
            var patient2 = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202310000001");
            var op1 = new OperationRequest("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            var op2 = new OperationRequest("2025-01-01", "eletric", patient2.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            
            var op1Dto = new OperationRequestDto("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId");

            _mockService.Setup(service => service.GetOperationRequestsWithFilters(filters, It.IsAny<string>()))
                .ReturnsAsync(new List<OperationRequestDto> { op1Dto });

            // Act
            var result = await _controller.GetOperationRequests(filters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);
            Assert.Equal(1, returnValue.Count); // Verifique o número de elementos na lista
        }

        [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByPatientName_ReturnsRequests()
        {
            // Arrange
            var filters = new OperationRequestFilterDto { PatientName = "Jane Doe" };
            var patient = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");
            var patient2 = new Patient("Jane Doe2", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202310000001");
            var op1 = new OperationRequest("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            var op2 = new OperationRequest("2025-01-01", "eletric", patient2.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");

            var op1Dto = new OperationRequestDto("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId");

            _mockService.Setup(service => service.GetOperationRequestsWithFilters(It.IsAny<OperationRequestFilterDto>(), It.IsAny<string>()))
                .ReturnsAsync(new List<OperationRequestDto> { op1Dto });

            // Act
            var result = await _controller.GetOperationRequests(filters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);
            Assert.Equal(1, returnValue.Count); // Verifique o número de elementos na lista
        }










    }









}
// using System;
// using System.Collections.Generic;
// using System.Threading.Tasks;
// using DDDSample1.Controllers;
// using DDDSample1.Domain.StaffMembers; // Ensure this matches your actual namespace
// using DDDSample1.Domain.Shared;
// using Microsoft.AspNetCore.Mvc;
// using Moq;
// using Xunit;

// namespace DDDSample1.Tests.UnitTests.Controllers
// {
//     public class StaffControllerTest
//     {
//         private Mock<IStaffService>? _mockService;
//         private StaffController? _controller;

//         [Fact]
//         public async Task Create_ReturnsCreatedAtActionResult_WhenStaffIsCreated()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             var dto = new CreatingStaffDto
//             {
//                 FullName = "Jane Doe",
//                 LicenseNumber = "123456",
//                 SpecializationId = "SpecializationId",
//                 Email = "jane.doe@example.com",
//                 PhoneNumber = "+351 1234567890",
//                 Category = "Doctor"
//             };
//             var staffDto = new StaffDto
//             {
//                 Id = Guid.NewGuid(),
//                 FullName = "Jane Doe",
//                 LicenseNumber = "123456",
//                 SpecializationId = "SpecializationId",
//                 Email = "jane.doe@example.com",
//                 PhoneNumber = "+351 1234567890",
//                 Category = "Doctor"
//             };
//             _mockService.Setup(service => service.CreateAsync(dto)).ReturnsAsync(staffDto);

//             // Act
//             var result = await _controller.Create(dto);
            
//             // Assert
//             var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
//             Assert.Equal(nameof(_controller.GetById), actionResult.ActionName);
//             Assert.Equal(staffDto, actionResult.Value);
//         }

//         [Fact]
//         public async Task Create_ReturnsBadRequest_WhenExceptionIsThrown()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             var dto = new CreatingStaffDto
//             {
//                 FullName = "Jane Doe",
//                 LicenseNumber = "123456",
//                 SpecializationId = "SpecializationId",
//                 Email = "jane.doe@example.com",
//                 PhoneNumber = "+351 1234567890",
//                 Category = "Doctor"
//             };
//             _mockService.Setup(service => service.CreateAsync(dto)).ThrowsAsync(new Exception("Error"));

//             // Act
//             var result = await _controller.Create(dto);

//             // Assert
//             var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
//             var value = actionResult.Value;
//             Assert.NotNull(value);
//             Assert.Equal("Error", value.GetType().GetProperty("Message")?.GetValue(value));
//         }

//         [Fact]
//         public async Task Update_ReturnsOkResult_WhenStaffIsUpdated()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             var dto = new EditingStaffDto { LicenseNumber = "123456" };
//             var staffDto = new StaffDto
//             {
//                 Id = Guid.NewGuid(),
//                 FullName = "Jane Doe",
//                 LicenseNumber = "123456",
//                 SpecializationId = "SpecializationId",
//                 Email = "jane.doe@example.com",
//                 PhoneNumber = "+351 1234567890",
//                 Category = "Doctor"
//             };
//             _mockService.Setup(service => service.UpdateAsync(dto)).ReturnsAsync(staffDto);

//             // Act
//             var result = await _controller.Update("123456", dto);

//             // Assert
//             var actionResult = Assert.IsType<OkObjectResult>(result.Result);
//             Assert.Equal(staffDto, actionResult.Value);
//         }

//         [Fact]
//         public async Task Delete_ReturnsNoContentResult_WhenStaffIsDeleted()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             _mockService.Setup(service => service.DeleteAsync("123456")).Returns(Task.CompletedTask);

//             // Act
//             var result = await _controller.Delete("123456");

//             // Assert
//             Assert.IsType<NoContentResult>(result);
//         }

//         [Fact]
//         public async Task GetAll_ReturnsAllStaffMembers()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             var staffMembers = new List<StaffDto> { new StaffDto(), new StaffDto() };
//             _mockService.Setup(service => service.GetAllAsync()).ReturnsAsync(staffMembers);

//             // Act
//             var result = await _controller.GetAll();

//             // Assert
//             var actionResult = Assert.IsType<ActionResult<IEnumerable<StaffDto>>>(result);
//             Assert.Equal(staffMembers, actionResult.Value);
//         }

//         [Fact]
//         public async Task GetById_ReturnsStaff_WhenStaffExists()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             var staffDto = new StaffDto();
//             _mockService.Setup(service => service.GetByIdAsync(It.IsAny<Guid>())).ReturnsAsync(staffDto);

//             // Act
//             var result = await _controller.GetById(Guid.NewGuid());

//             // Assert
//             var actionResult = Assert.IsType<ActionResult<StaffDto>>(result);
//             Assert.Equal(staffDto, actionResult.Value);
//         }

//         [Fact]
//         public async Task GetById_ReturnsNotFound_WhenStaffDoesNotExist()
//         {
//             _mockService = new Mock<IStaffService>();
//             _controller = new StaffController(_mockService.Object);

//             // Arrange
//             _mockService.Setup(service => service.GetByIdAsync(It.IsAny<Guid>())).ReturnsAsync((StaffDto?)null);

//             // Act
//             var result = await _controller.GetById(Guid.NewGuid());

//             // Assert
//             Assert.IsType<NotFoundResult>(result.Result);
//         }
//     }
// }
