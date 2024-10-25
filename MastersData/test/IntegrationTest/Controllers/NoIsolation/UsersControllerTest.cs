using System.Net;
using System.Net.Http.Json;
using DDDSample1.Domain.User;
using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Xunit.Abstractions;

namespace DDDSample1.Tests.IntegrationTests.Controllers.NoIsolation
{
    public class UsersControllerTest :
        IClassFixture<MastersDataWebApplicationFactory<Program>>
    {
        private readonly HttpClient _client;
        private readonly MastersDataWebApplicationFactory<Program> _factory;
        private readonly ITestOutputHelper _output;

        public UsersControllerTest(MastersDataWebApplicationFactory<Program> factory, ITestOutputHelper output)
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
                Email = "jane.doe@gmail.com",
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
        public async Task RegisterPatient_ReturnData()
        {
            // Arrange
            using (var scope = _factory.Services.CreateScope())
            {
                var services = scope.ServiceProvider;
                var context = services.GetRequiredService<DDDSample1DbContext>();
                context.Database.EnsureCreated();
                Utilities.InitializeDbForTests(context);
            }

            // Act
            var response = await _client.PostAsJsonAsync("/api/users/patients", new RegisteringPatientDto
            {
                Email = "john.doe@gmail.com",
                Name = "John Doe",
                PhoneNumber = "+351 123456789",
                Street = "address1",
                PostalCode = "1234-123",
                City = "city",
                Country = "country",
                Password = "TestPassword123/",
            });

            // Assert
            response.EnsureSuccessStatusCode();
        }

        [Fact]
        public async Task EditPatient_ReturnData()
        {
            // Arrange
            using (var scope = _factory.Services.CreateScope())
            {
                var services = scope.ServiceProvider;
                var context = services.GetRequiredService<DDDSample1DbContext>();
                context.Database.EnsureCreated();
                Utilities.InitializeDbForTests(context);
            }

            var token = await GetAuthTokenAsync();
            _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

            // Act
            var response = await _client.PatchAsJsonAsync("/api/users/patients/edit", new EditingPatientDto
            {
                Email = "jane.doe@gmail.com",
                NameToEdit = "Jane Doing"
            });

            // Assert
            response.EnsureSuccessStatusCode();
        }
    }
}

        