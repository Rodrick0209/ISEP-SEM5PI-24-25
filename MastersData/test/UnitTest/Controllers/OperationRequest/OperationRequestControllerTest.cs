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
    public class OperationRequestControllerTests
    {

    private readonly Mock<IOperationRequestService> _mockService;
    private readonly OperationRequestController _controller;
        
        public OperationRequestControllerTests()
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