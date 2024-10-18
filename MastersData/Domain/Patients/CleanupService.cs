using System;
using System.Threading;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using Microsoft.Extensions.Hosting;

public class CleanupService : BackgroundService
{
    private readonly PatientService _patientService;
    private readonly TimeSpan _retentionPeriod = TimeSpan.FromDays(30); // Set your retention period

    public CleanupService(PatientService patientService)
    {
        _patientService = patientService;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(TimeSpan.FromDays(1), stoppingToken); // Adjust the frequency as needed

            await _patientService.CleanupOldPatientLogs(_retentionPeriod);
        }
    }
}