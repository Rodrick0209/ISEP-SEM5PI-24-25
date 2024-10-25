using System.Net;
using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Authentication.OAuth;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Xunit.Abstractions;

namespace DDDSample1.Tests.IntegrationTests.Controllers.NoIsolation
{
    public class PatientsControllerTest :
        IClassFixture<MastersDataWebApplicationFactory<Program>>
    {
        private readonly HttpClient _client;
        private readonly MastersDataWebApplicationFactory<Program> _factory;
        private readonly ITestOutputHelper _output;

        public PatientsControllerTest(MastersDataWebApplicationFactory<Program> factory, ITestOutputHelper output)
        {
            _factory = factory;
            _client = factory.CreateClient(new WebApplicationFactoryClientOptions
            {
                AllowAutoRedirect = false
            });
            _output = output;
        }

        private async Task<string> GetAuthTokenAsync()
        {
            var loginRequest = new LoginRequest
            {
                Email = "admin@teste.com",
                Password = "password"
            };


            var response = await _client.PostAsJsonAsync("/api/login/login", loginRequest);

            _output.WriteLine($"Status da resposta de login: {response.StatusCode}");
            if (response.StatusCode == HttpStatusCode.Unauthorized)
            {
                var errorContent = await response.Content.ReadAsStringAsync();
                _output.WriteLine($"Erro de login: {errorContent}");
            }

            response.EnsureSuccessStatusCode();

            var responseString = await response.Content.ReadAsStringAsync();
            _output.WriteLine($"Resposta de login: {responseString}");
            var token = Newtonsoft.Json.JsonConvert.DeserializeObject<AuthTokenResponse>(responseString).token;

            return token;
        }

        private class AuthTokenResponse
        {
            public string token { get; set; }
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

            var token = await GetAuthTokenAsync();
            _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

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
            // Arrange
            var token = await GetAuthTokenAsync();
            _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

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

            var token = await GetAuthTokenAsync();
            _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

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

            var token = await GetAuthTokenAsync();
            _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

            // Act
            var response = await _client.DeleteAsync("/api/patients/202410000001");

            // Assert
            response.EnsureSuccessStatusCode();
        }

    }


}
