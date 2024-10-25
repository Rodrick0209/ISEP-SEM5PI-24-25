using System.Net.Http.Json;
using DDDSample1.Domain.OperationRequest;

using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using System.Text.Json;
using Xunit.Abstractions;
using System.Text;
using System.Net;



namespace DDDSample1.Tests.IntegrationTests.Controllers
{ 

public class OperationRequestControllerTestWithoutIsolation :
    IClassFixture<MastersDataWebApplicationFactory<Program>>
{

    
    private readonly HttpClient _client;
    private readonly MastersDataWebApplicationFactory<Program> _factory;
        private readonly ITestOutputHelper _output;


    public OperationRequestControllerTestWithoutIsolation(MastersDataWebApplicationFactory<Program> factory,ITestOutputHelper output)
    {
        _factory = factory;
        _client = factory.CreateClient(new WebApplicationFactoryClientOptions
        {
            AllowAutoRedirect = false
        });
        _output = output;
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
            Utilities.InitializeDbForTests(context);
        }

        var token = await GetAuthTokenAsync();
        _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);
                _output.WriteLine($"Token: {token}");
         
            // Act
            var response = await _client.GetAsync("/api/operationrequest/getWithFilters");

            // Assert
            response.EnsureSuccessStatusCode();
    }

    private async Task<string> GetAuthTokenAsync()
    {
         var loginRequest = new LoginRequest
        {
            Email = "D202512345@gmail.com",
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
    public async Task CreateOperationRequest_ReturnsSuccessStatusCode()
    {
        // Arrange 
        string? operationTypeId = null; 
        string? patientId = null;
        string? doctorId = null;
        
        using (var scope = _factory.Services.CreateScope())
        {
            var services = scope.ServiceProvider;
            var context = services.GetRequiredService<DDDSample1DbContext>();
            context.Database.EnsureCreated();
            Utilities.InitializeDbForTests(context);

            var operationType = context.OperationTypes.FirstOrDefault();
            if (operationType != null)
            {
                operationTypeId = operationType.Id.AsString();
            }

            var patient = context.Patients.FirstOrDefault();
            if (patient != null)
            {
                patientId = patient.Id.AsString();
            }

            var doctor = context.StaffMembers.FirstOrDefault();
            if (doctor != null)
            {
                doctorId = doctor.Id.AsString();
            }
        }

        // Verifique se todos os IDs necessários foram atribuídos
        if (operationTypeId == null || patientId == null || doctorId == null)
        {
            throw new InvalidOperationException("One or more required IDs are null.");
        }

        var token = await GetAuthTokenAsync();
        _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);


        // Act
        var response = await _client.PostAsJsonAsync("/api/operationrequest", new OperationRequestDto(
            "2025-01-01",
            "eletric",
            patientId,
            operationTypeId,
            doctorId
        ));

        _output.WriteLine($"Response status code: {response.StatusCode}");

        // Assert
        response.EnsureSuccessStatusCode();

        var responseString = await response.Content.ReadAsStringAsync();
        Assert.Contains("2025-01-01", responseString);
        Assert.Contains("eletric", responseString);
        Assert.Contains(patientId, responseString);
        Assert.Contains(operationTypeId, responseString);
        Assert.Contains(doctorId, responseString);

    }


[Fact]
public async Task UpdateOperationRequest_ReturnsSuccessStatusCode()
{
    // Arrange
    Guid? operationRequestId = null;

    using (var scope = _factory.Services.CreateScope())
    {
        var services = scope.ServiceProvider;
        var context = services.GetRequiredService<DDDSample1DbContext>();
        context.Database.EnsureCreated();
        Utilities.InitializeDbForTests(context);
  
        var operationRequest = context.OperationRequests.FirstOrDefault();
        if (operationRequest != null)
        {
            operationRequestId = operationRequest.Id.AsGuid();
        }
    }

    if (operationRequestId == null)
    {
        throw new InvalidOperationException("Id null.");
    }

    var token = await GetAuthTokenAsync();
    _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);
    _output.WriteLine($"OperationRequestId: {operationRequestId}");

    // Act
    var changeOperationRequestDto = new ChangeOperationRequestDto(
        (Guid)operationRequestId,
        "2029-01-01",
        "eletric"
    );

    var content = new StringContent(
        JsonSerializer.Serialize(changeOperationRequestDto), Encoding.UTF8, "application/json"
    );

    var response = await _client.PutAsync($"/api/operationrequest/{operationRequestId}", content);

    // Assert
    response.EnsureSuccessStatusCode();
    var responseString = await response.Content.ReadAsStringAsync();
    Assert.Contains("2029-01-01", responseString);
    Assert.Contains("eletric", responseString);
}

[Fact]
public async Task UpdateOperationRequest_ReturnsUnsuccessStatusCode()
{
    // Arrange
    Guid? operationRequestId = null;

    using (var scope = _factory.Services.CreateScope())
    {
        var services = scope.ServiceProvider;
        var context = services.GetRequiredService<DDDSample1DbContext>();
        context.Database.EnsureCreated();
        Utilities.InitializeDbForTests(context);
   
        var operationRequest = context.OperationRequests.FirstOrDefault();
        if (operationRequest != null)
        {
            operationRequestId = operationRequest.Id.AsGuid();
        }
    }

    if (operationRequestId == null)
    {
        throw new InvalidOperationException("Id null.");
    }

    var token = await GetAuthTokenAsync();
    _client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);
    _output.WriteLine($"OperationRequestId: {operationRequestId}");

    // Act
    var changeOperationRequestDto = new ChangeOperationRequestDto(
        (Guid)operationRequestId,
        "2029-01-01",
        "falha"
    );

    var content = new StringContent(
        JsonSerializer.Serialize(changeOperationRequestDto), Encoding.UTF8, "application/json"
    );

    var response = await _client.PutAsync($"/api/operationrequest/{operationRequestId}", content);

    var responseContent = await response.Content.ReadAsStringAsync();

    // Assert
    Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
    Assert.Contains("Invalid priority", responseContent);
}






}


}




