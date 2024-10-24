using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;

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
            context.Database.EnsureCreated();

            var patient = new Patient("John", "Doe", "John doe", "john.doe@example.com", "+351 123456789", "Address 1", "1234-123", "City", "Country", "Emergency Contact", "Emergency Contact Email", "+351 123456789", "202410000001");
            context.Patients.Add(patient);
        }

        // Act
        var response = await _client.GetAsync("/api/patients");

        // Assert
        response.EnsureSuccessStatusCode();
        var responseString = await response.Content.ReadAsStringAsync();
        Assert.Contains("John", responseString);
    }

}