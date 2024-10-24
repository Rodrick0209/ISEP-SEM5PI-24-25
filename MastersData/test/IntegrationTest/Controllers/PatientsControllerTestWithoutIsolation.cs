using System.Text;
using System.Text.Json;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class PatientsControllerTestWithoutIsolation :
        IClassFixture<MastersDataWebApplicationFactory<Program>>
    {
        private readonly HttpClient _client;
        private readonly MastersDataWebApplicationFactory<Program> _factory;

        public PatientsControllerTestWithoutIsolation(MastersDataWebApplicationFactory<Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient(new WebApplicationFactoryClientOptions
            {
                AllowAutoRedirect = false
            });
        }

        [Fact]
        public async Task Get_ReturnData()
        {
            // Arrange
            using (var scope = _factory.Services.CreateScope())
            {
                var services = scope.ServiceProvider;
                var context = services.GetRequiredService<DDDSample1DbContext>();

                Utilities.InitializeDbForTests(context);
            }

            // Act
            var response = await _client.GetAsync("/api/patients");

            // Assert
            response.EnsureSuccessStatusCode();
            var responseString = await response.Content.ReadAsStringAsync();
            Assert.Contains("John", responseString);
        }

        [Fact]
        public async Task CreatePatient_ReturnsSuccessStatusCode()
        {
            // Act
            var response = await _client.PostAsync("/api/patients", new StringContent(
                JsonSerializer.Serialize(new CreatingPatientProfileDto
                {
                    FirstName = "Jane",
                    LastName = "Doe",
                    FullName = "Jane Doe",
                    DateOfBirth = "1990-01-01",
                    Gender = "female",
                    Email = "jane.doe@example.com",
                    PhoneNumber = "+351 123-456-7890",
                    Street = "123 Main St",
                    PostalCode = "12345",
                    City = "Anytown",
                    Country = "Anycountry",
                    EmergencyContactName = "John Doe",
                    EmergencyContactEmail = "john.doe@example.com",
                    EmergencyContactPhoneNumber = "+351 098-765-4321",
                }), Encoding.UTF8, "application/json"));

            // Assert
            response.EnsureSuccessStatusCode();
            var responseString = await response.Content.ReadAsStringAsync();
            Assert.Contains("Jane", responseString);
        }

        [Fact]
        public async Task UpdatePatient_ReturnsSuccessStatusCode()
        {
            // Arrange
            using (var scope = _factory.Services.CreateScope())
            {
                var services = scope.ServiceProvider;
                var context = services.GetRequiredService<DDDSample1DbContext>();

                Utilities.InitializeDbForTests(context);
            }

            // Act
            var response = await _client.PatchAsync("/api/patients/202410000001", new StringContent(
                JsonSerializer.Serialize(new EditingPatientProfileDto
                {
                    MedicalRecordNumber = "202410000001",
                    FullName = "Jane Doe",
                }
                ), Encoding.UTF8, "application/json"));

            // Assert
            response.EnsureSuccessStatusCode();
            var responseString = await response.Content.ReadAsStringAsync();
            Assert.Contains("Jane", responseString);
        }

        [Fact]
        public async Task DeletePatient_ReturnsSuccessStatusCode()
        {
            // Arrange
            using (var scope = _factory.Services.CreateScope())
            {
                var services = scope.ServiceProvider;
                var context = services.GetRequiredService<DDDSample1DbContext>();

                Utilities.InitializeDbForTests(context);
            }

            // Act
            var response = await _client.DeleteAsync("/api/patients/202410000001");

            // Assert
            response.EnsureSuccessStatusCode();
        }
    }
}
