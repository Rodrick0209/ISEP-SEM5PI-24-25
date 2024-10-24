using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure;
using DDDSample1.Startup;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;

public class OperationRequestControllerWithoutIsolationTest :
    IClassFixture<MastersDataWebApplicationFactory<Program>>
{

    
    private readonly HttpClient _client;
    private readonly MastersDataWebApplicationFactory<Program> _factory;

    public OperationRequestControllerWithoutIsolationTest(MastersDataWebApplicationFactory<Program> factory)
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
            // Criar e adicionar o paciente
           
           
            var johnCena = new Patient(
                "John Cena",
                "2022-10-01",
                "male",
                "john.cena5@example.com",
                "+351 123412421",
                "Main Street 123",
                "1234-567",
                "Los Angeles",
                "USA",
                "Jane Cena",
                "jane.cena@example.com",
                "+351 234567234",
                MedicalRecordNumberGenerator.GenerateMedicalRecordNumber(null)
            );
            context.Patients.Add(johnCena);

            Console.WriteLine("ERRO CHECKPOINT 2");    


            // Criar e adicionar as especializações
            var specialization1 = new Specialization("Ortopedia");
            var specialization2 = new Specialization("Oncologia");
            var specialization3 = new Specialization("Obstetricia");
            context.Specializations.AddRange(specialization1, specialization2, specialization3);

            Console.WriteLine("ERRO CHECKPOINT 3");    


            // Criar o staff necessário
            var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
            var requiredStaff2 = new RequiredStaff(20, specialization2.Id);
            var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
            var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization2.Id) };
            var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization2.Id) };

            // Criar as fases da operação
            var phase1 = new Phase(20, requiredStaffList1);
            var phase2 = new Phase(90, requiredStaffList2);
            var phase3 = new Phase(15, requiredStaffList3);
            Console.WriteLine("ERRO CHECKPOINT 4");    


            // Criar e adicionar o tipo de operação
            var operationType = new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id);
            context.OperationTypes.Add(operationType);

            // Persistir todas as entidades relacionadas
            await context.SaveChangesAsync();

            Console.WriteLine("ERRO CHECKPOINT 5");    


            // Criar e adicionar o OperationRequest
            var operationRequest = new OperationRequest(
                "2025-02-18", 
                "emergency", 
                johnCena.Id.AsString(), 
                operationType.Id.AsString(), 
                new StaffId("D202512345").AsString(), 
                new StaffId("D202512344").AsString()
            );
            context.OperationRequests.Add(operationRequest);

            await context.SaveChangesAsync();
        }

        // Act
        var response = await _client.GetAsync("/api/operationRequest");

        // Assert
        response.EnsureSuccessStatusCode();
        var responseString = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseString);
    }









}