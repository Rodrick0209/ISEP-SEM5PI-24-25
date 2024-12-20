
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.User;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Domain.RoomTypes;
using Org.BouncyCastle.Asn1.Pkcs;
using System.Net.Http;
using System.Text;
using Newtonsoft.Json;

public static class DataSeeder
{
  public static async Task SeedAsync(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

    Patient johnCena = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "+351 123456123",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000001");

    Patient johnCena2 = new Patient(
      "John Cena2",
      "2022-10-01",
      "male",
      "john.cena2@example.com",
      "+351 123456121",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000002");

    Patient johnCena3 = new Patient(
      "John Cena3",
      "2022-10-01",
      "male",
      "john.cena3@example.com",
      "+351 123456621",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000003");

    Patient johnCena4 = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena4@example.com",
      "+351 123412621",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000004");

    Patient johnCena5 = new Patient(
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
      "202410000005");
    Patient rodrick = new Patient(
      "Rodrick",
      "2022-10-01",
      "male",
      "1221083@isep.ipp.pt",
      "+351 123456788",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@gmail.com",
      "+351 234567234",
      "202410000006");

    var patients = new List<Patient> { johnCena, johnCena2, johnCena3, johnCena4, johnCena5, rodrick };

    context.Patients.AddRange(patients);

    foreach (var patient in patients)
    {
      await CreateMedicalRecordForPatient(patient);
    }


    var specialization1 = new Specialization("Ortopedia");
    var specialization2 = new Specialization("Oncologia");
    var specialization3 = new Specialization("Obstetricia");
    var specialization4 = new Specialization("Enfermeiro ambulante");

    var specializations = new List<Specialization> { specialization1, specialization2, specialization3,specialization4 };

    context.Specializations.AddRange(specializations);

    // Exemplo: Convertendo a data para uma string válida antes de passar para AvailabilitySlot
    string date = "2025-01-01"; // ou extraído de outra fonte como DateTime.Now.ToString("yyyy-MM-dd")
    string startTime = "08:00";
    string endTime = "12:00";


    // Create required staff

    var requiredStaffList1 = new List<RequiredStaff> { new RequiredStaff(1, specialization1.Id), new RequiredStaff(2,specialization4.Id) }; // New instance
    var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(1, specialization2.Id) }; // New instance
    var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(1, specialization2.Id) }; // New instance

    PasswordHasher hasher = new PasswordHasher();
    string password = hasher.HashPassword("password");
    var user = new User("D202512345@gmail.com", "doctor", password);
    var user2 = new User("D202512344@gmail.com", "doctor", password);
    var user3 = new User("admin@teste.com", "admin", password);
    var user4 = new User("john.cena@example.com", "patient", password);
    var user5 = new User("1221083@isep.ipp.pt", "patient", password);
    var user6 = new User("rodrigopontescardoso@gmail.com", "admin", password);

    var users = new List<User> { user, user2, user3, user4, user5, user6 };

    context.Users.AddRange(users);

    // Create new phases with required staff
    var phase1 = new Phase(20, requiredStaffList1);
    var phase2 = new Phase(90, requiredStaffList2);
    var phase3 = new Phase(15, requiredStaffList3);
    var phases = new List<Phase> { phase1, phase2, phase3 };

    context.Phases.AddRange(phases);

    // Create a new operation type with the phases and specialization
    var operationType = new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id);
    var operationType2 = new OperationType("New Operation Type2", true, phase1, phase2, phase3, specialization2.Id);
    var operationTypes = new List<OperationType> { operationType, operationType2 };

    context.OperationTypes.AddRange(operationTypes);

    var operationRequest = new OperationRequest("2025-02-18", "emergency", johnCena.Id.AsString(), operationType.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512344").AsString());
    operationRequest.staffAssignedSurgery.addStaffSurgeryPhase(new StaffId("D202512345"));
    operationRequest.staffAssignedSurgery.addStaffSurgeryPhase(new StaffId("D202512346"));
    operationRequest.staffAssignedSurgery.addStaffAnesthesyPhase(new StaffId("D202512344"));
    SeedOperationRequest(context, operationRequest);
    var staffAssignedSurgery = operationRequest.staffAssignedSurgery;
    SeedStaffAssignedSurgery(context, staffAssignedSurgery);

    var operationRequest2 = new OperationRequest("2025-02-19", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest2);

    var operationRequest3 = new OperationRequest("2025-02-20", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest3);

    var operationRequest4 = new OperationRequest("2025-02-23", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest4);

    var operationRequest5 = new OperationRequest("2025-02-24", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest5);
    var operationRequest6 = new OperationRequest("2025-02-25", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest6);
    var operationRequest7 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest7);
    var operationRequest8 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest8);

    var operationRequest9 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest9);

    var operationRequest10 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest10);

    var operationRequest11 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest11);

    var operationRequest12 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest12);

    var operationRequest13 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest13);


    Staff staff = new Staff(new StaffId("D202512345"), "staff", "12345", specialization1.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff3 = new Staff(new StaffId("D202512340"), "staff", "12345", specialization1.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff4 = new Staff(new StaffId("D202512341"), "staff", "12345", specialization1.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff5 = new Staff(new StaffId("D202512310"), "staff", "12345", specialization2.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff6 = new Staff(new StaffId("D202512311"), "staff", "12345", specialization2.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff7 = new Staff(new StaffId("D202512312"), "staff", "12345", specialization2.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff8 = new Staff(new StaffId("D202512313"), "staff", "12345", specialization2.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff9 = new Staff(new StaffId("D202512314"), "staff", "12345", specialization4.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff10 = new Staff(new StaffId("D202512315"), "staff", "12345", specialization4.Id, "email@gmail.com", "+951999999999", "Doctor");
    Staff staff11 = new Staff(new StaffId("D202512316"), "staff", "12345", specialization4.Id, "email@gmail.com", "+951999999999", "Doctor");





    SeedStaff(context, staff);
    SeedStaff(context, staff3);
    SeedStaff(context, staff4);
    SeedStaff(context, staff5);
    SeedStaff(context, staff6);
    SeedStaff(context, staff7);
    SeedStaff(context, staff8);
    SeedStaff(context, staff9);
    SeedStaff(context, staff10);
    SeedStaff(context, staff11);

    var availableSlot2 = new AvailabilitySlot(staff.Id.AsString());
    DailyAvailability dailyAvailability = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability);
    availableSlot2.Availability.Add(dailyAvailability);
    SeedAvailabilitySlots(context, availableSlot2);


    var availableSlot3 = new AvailabilitySlot(staff3.Id.AsString());
    DailyAvailability dailyAvailability2 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability2);
    availableSlot3.Availability.Add(dailyAvailability2);
    SeedAvailabilitySlots(context, availableSlot3);


    var availableSlot4 = new AvailabilitySlot(staff4.Id.AsString());
    DailyAvailability dailyAvailability3 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability3);
    availableSlot4.Availability.Add(dailyAvailability3);
    SeedAvailabilitySlots(context, availableSlot4);


    var availableSlot5 = new AvailabilitySlot(staff5.Id.AsString());
    DailyAvailability dailyAvailability4 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability4);
    availableSlot5.Availability.Add(dailyAvailability4);
    SeedAvailabilitySlots(context, availableSlot5);

    var availableSlot6 = new AvailabilitySlot(staff6.Id.AsString());
    DailyAvailability dailyAvailability5 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability5);
    availableSlot6.Availability.Add(dailyAvailability5);
    SeedAvailabilitySlots(context, availableSlot6);


    var availableSlot7 = new AvailabilitySlot(staff7.Id.AsString());
    DailyAvailability dailyAvailability6 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability6);
    availableSlot7.Availability.Add(dailyAvailability6);
    SeedAvailabilitySlots(context, availableSlot7);


    var availableSlot8 = new AvailabilitySlot(staff8.Id.AsString());
    DailyAvailability dailyAvailability7 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability7);
    availableSlot8.Availability.Add(dailyAvailability7);
    SeedAvailabilitySlots(context, availableSlot8);

    var availableSlot9 = new AvailabilitySlot(staff9.Id.AsString());
    DailyAvailability dailyAvailability8 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability8);
    availableSlot9.Availability.Add(dailyAvailability8);
    SeedAvailabilitySlots(context, availableSlot9);

    var availableSlot10 = new AvailabilitySlot(staff10.Id.AsString());
    DailyAvailability dailyAvailability9 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability9);
    availableSlot10.Availability.Add(dailyAvailability9);
    SeedAvailabilitySlots(context, availableSlot10);

    var availableSlot11= new AvailabilitySlot(staff11.Id.AsString());
    DailyAvailability dailyAvailability10 = new DailyAvailability(new DateOnly(2026, 12, 30));
    SeedDailyAvailability(context, dailyAvailability10);
    availableSlot11.Availability.Add(dailyAvailability10);
    SeedAvailabilitySlots(context, availableSlot11);



    
    var roomType = new RoomType("R12-2354", "RoomType1", "RoomType1", true);
    context.RoomTypes.Add(roomType);

    var operationRoom = new OperationRoom("or3", roomType, "10");
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 720, 840);
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 1080, 1200);
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 1200, 1300);
    SeedOperationRoom(context, operationRoom);


    OperationRoom teste = new OperationRoom("or8", roomType, "10");

    var rooms = new List<OperationRoom>
    {
      new OperationRoom("or2", roomType, "10"),
      new OperationRoom("or1", roomType, "10"),
      new OperationRoom("or4", roomType, "10"),
      new OperationRoom("or5", roomType, "10"),
      new OperationRoom("or6", roomType, "10"),
      new OperationRoom("or7", roomType, "10"),
      teste,
      new OperationRoom("or9", roomType, "10"),
      new OperationRoom("or10", roomType, "10")
    };

    context.OperationRooms.AddRange(rooms);

    context.SaveChanges();

    int currentHourInMinutes = DateTime.Now.Hour * 60 + DateTime.Now.Minute;
    int startMinute = currentHourInMinutes + 1;
    int endMinute = Math.Min(currentHourInMinutes + 120, 1440);
    var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day), new TimeSlot(startMinute, endMinute));
    var appointmentToday = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);


    var appointment = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 12), new TimeSlot(20, 110)), operationRoom.Id, operationRequest.Id);
    var appointment2 = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 12), new TimeSlot(720, 840)), operationRoom.Id, operationRequest2.Id);
    var appointment3 = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 13), new TimeSlot(720, 840)), operationRoom.Id, operationRequest3.Id);
    var appointment4 = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 14), new TimeSlot(720, 840)), operationRoom.Id, operationRequest4.Id);
    var appointment5 = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 15), new TimeSlot(720, 840)), operationRoom.Id, operationRequest5.Id);
    var appointment6 = new Appointment(new AppointmentTimeSlot(new DateOnly(2025, 11, 16), new TimeSlot(720, 840)), operationRoom.Id, operationRequest6.Id);

    SeedAppointments(context, appointment);
    Console.WriteLine("Appointment created WITH id " + appointment.Id.AsString());
    SeedAppointments(context, appointment2);
    SeedAppointments(context, appointment3);
    SeedAppointments(context, appointment4);
    SeedAppointments(context, appointment5);
    SeedAppointments(context, appointment6);
    SeedAppointments(context, appointmentToday);
    context.SaveChanges();
  }

  private static void SeedOperationRequest(DDDSample1DbContext context, OperationRequest operationRequest)
  {
    if (!context.OperationRequests.Any())
    {
      context.OperationRequests.Add(operationRequest);
    }
  }

  private static void SeedAvailabilitySlots(DDDSample1DbContext context, AvailabilitySlot availabilitySlot)
  {
    if (!context.AvailabilitySlots.Any())
    {
      context.AvailabilitySlots.Add(availabilitySlot);
    }
  }


  private static void SeedDailyAvailability(DDDSample1DbContext context, DailyAvailability dailyAvailability)
  {
    if (!context.DailyAvailabilities.Any())
    {
      context.DailyAvailabilities.Add(dailyAvailability);
    }
  }

  private static void SeedUsers(DDDSample1DbContext context, User user, string pass)
  {
    PasswordHasher passwordHasher = new PasswordHasher();
    string password = passwordHasher.HashPassword(pass);
    user.SetPassword(password);
    if (!context.Users.Any())
    {
      context.Users.Add(user);
    }
  }

  private static void SeedOperationType(DDDSample1DbContext context, OperationType operationType)
  {
    if (!context.OperationTypes.Any())
    {
      context.OperationTypes.Add(operationType);
    }
  }

  private static void SeedPatients(DDDSample1DbContext context, Patient patient)
  {
    if (!context.Patients.Any())
    {
      context.Patients.Add(patient);
    }
  }

  private static void SeedStaffAssignedSurgery(DDDSample1DbContext context, StaffAssignedSurgery staffAssignedSurgery)
  {
    if (!context.StaffAssignedSurgeries.Any())
    {
      context.StaffAssignedSurgeries.Add(staffAssignedSurgery);
    }
  }

  private static void SeedSpecializations(DDDSample1DbContext context, Specialization specialization)
  {
    if (!context.Specializations.Any())
    {
      context.Specializations.Add(specialization);
    }
  }


  public static void SeedStaff(DDDSample1DbContext context, Staff staff)
  {
    if (!context.Specializations.Any())
    {
      context.StaffMembers.Add(staff);
    }
  }
  private static async Task CreateMedicalRecordForPatient(Patient patient)
  {
    using (var httpClient = new HttpClient())
    {
      var medicalRecordDto = new
      {
        patientId = patient.MedicalRecordNumber._medicalRecordNumber,
        allergies = new string[] { },
        medicalConditions = new string[] { }
      };

      var content = new StringContent(JsonConvert.SerializeObject(medicalRecordDto), Encoding.UTF8, "application/json");

      try
      {
        var response = await httpClient.PostAsync("http://localhost:4000/api2/medicalRecord/create", content);
        response.EnsureSuccessStatusCode();
        Console.WriteLine("Medical record created");

      }
      catch (Exception ex)
      {
        Console.WriteLine($"Failed to create medical record for patient {patient.MedicalRecordNumber}: {ex.Message}");
      }
    }
  }

  public static void SeedOperationRoom(DDDSample1DbContext context, OperationRoom operationRoom)
  {
    if (!context.OperationRooms.Any())
    {
      context.OperationRooms.Add(operationRoom);
    }
  }

  public static void SeedAppointments(DDDSample1DbContext context, Appointment appointment)
  {
    if (!context.Appointments.Any())
    {
      context.Appointments.Add(appointment);
    }
  }



  public static async Task UnseedAsync(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

    // Remove appointments
    var appointments = context.Appointments.ToList();
    context.Appointments.RemoveRange(appointments);

    // Remove availability slots
    var availabilitySlots = context.AvailabilitySlots.ToList();
    context.AvailabilitySlots.RemoveRange(availabilitySlots);

    // Remove operation rooms
    var operationRooms = context.OperationRooms.ToList();
    context.OperationRooms.RemoveRange(operationRooms);

    // Remove staff
    var staffMembers = context.StaffMembers.ToList();
    context.StaffMembers.RemoveRange(staffMembers);

    // Remove operation requests
    var operationRequests = context.OperationRequests.ToList();
    context.OperationRequests.RemoveRange(operationRequests);

    // Remove operation types
    var operationTypes = context.OperationTypes.ToList();
    context.OperationTypes.RemoveRange(operationTypes);

    // Remove phases
    var phases = context.Phases.ToList();
    context.Phases.RemoveRange(phases);

    // Remove specializations
    var specializations = context.Specializations.ToList();
    context.Specializations.RemoveRange(specializations);

    // Remove patients
    var patients = context.Patients.ToList();
    context.Patients.RemoveRange(patients);

    // Remove users
    var users = context.Users.ToList();
    context.Users.RemoveRange(users);

    // Save the changes to the database
    await context.SaveChangesAsync();
  }


}
