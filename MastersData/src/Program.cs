using Microsoft.AspNetCore.Builder;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Identity.UI.Services;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Pomelo.EntityFrameworkCore.MySql.Infrastructure;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.Shared;

using Microsoft.AspNetCore.Identity;
using DDDSample1.Infrastructure.Users;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.IdentityModel.Tokens;
using System;
using System.Text;
using Microsoft.OpenApi.Models;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Infrastructure.OperationRequests;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Infrastructure.OperationTypes;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.StaffMembers;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure.Patients;
using DDDSample1.Infrastructure.Specializations;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Infrastructure.OperationRequestLoggers;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Infrastructure.PatientLoggers;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Infrastructure.AvailabilitySlots;
using DDDSample1.Domain.StaffLoggers;
using DDDSample1.Infrastructure.StaffLoggers;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Infrastructure.OperationRooms;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication.Google;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication.BearerToken;
using DDDSample1.Domain.Appointments;
using DDDSample1.Infrastructure.Appointments;
using Microsoft.Extensions.Configuration;




namespace DDDSample1.Startup
{

    public class Program
    {
        public static async Task Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);

            builder.Services.AddEndpointsApiExplorer();
            builder.Services.AddSwaggerGen(options =>
            {
                // Definição para JWT
                options.AddSecurityDefinition(JwtBearerDefaults.AuthenticationScheme, new OpenApiSecurityScheme
                {
                    Name = "Authorization",
                    Description = "Enter the Bearer Authorization : 'Bearer Generated-JWT-Token'",
                    In = ParameterLocation.Header,
                    Type = SecuritySchemeType.ApiKey,
                    Scheme = "Bearer"
                });
                // Adicionando requisitos de segurança para ambos
                options.AddSecurityRequirement(new OpenApiSecurityRequirement
                {
                    {
                        new OpenApiSecurityScheme
                        {
                            Reference = new OpenApiReference
                            {
                                Type = ReferenceType.SecurityScheme,
                                Id = JwtBearerDefaults.AuthenticationScheme
                            }
                        },
                        new List<string>()
                    }
                });
            });



            //CRIAR INTERFACE PARA TOKEN PROVIDER
            builder.Services.AddSingleton<TokenProvider>();
            // Add services to the container.
            var useInMemoryDatabase = builder.Configuration.GetValue<bool>("DatabaseSettings:UseInMemoryDatabase");
            var connectionString = builder.Configuration.GetConnectionString("DefaultConnection");

            builder.Services.AddDbContext<DDDSample1DbContext>(opt =>
            {
                if (useInMemoryDatabase)
                {
                    opt.UseInMemoryDatabase("DDDSample1DB");
                }
                else
                {
                    opt.UseMySql(connectionString, ServerVersion.AutoDetect(connectionString));
                }

                opt.ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>();
            });

            builder.Services.AddAuthorization();
            builder.Services.AddAuthentication(options =>
            {
                options.DefaultScheme = JwtBearerDefaults.AuthenticationScheme;
                options.DefaultChallengeScheme = JwtBearerDefaults.AuthenticationScheme;
            }).AddJwtBearer(options =>
            {
                options.RequireHttpsMetadata = false;
                options.SaveToken = true;
                options.TokenValidationParameters = new TokenValidationParameters
                {
                    ValidateIssuer = true,
                    ValidateAudience = true,
                    ValidateLifetime = true,
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(builder.Configuration["Jwt:SecretKey"]!)),
                    ValidIssuer = builder.Configuration["Jwt:Issuer"],
                    ValidAudience = builder.Configuration["Jwt:Audience"],
                    ClockSkew = TimeSpan.Zero // Reduzir a tolerância
                };
                // Personalização de erro 401
                options.Events = new JwtBearerEvents
                {
                    OnChallenge = context =>
                                {
                                    // Impedir a resposta padrão
                                    context.HandleResponse();
                                    context.Response.StatusCode = StatusCodes.Status401Unauthorized;
                                    context.Response.ContentType = "application/json";
                                    var result = JsonSerializer.Serialize(new { message = "Token de autenticação inválido ou expirado. Por favor, faça login novamente." });
                                    return context.Response.WriteAsync(result);
                                }
                };
            }).AddCookie(options =>
                {
                    options.LoginPath = "/google-login";
                })
            .AddGoogle(GoogleDefaults.AuthenticationScheme, options =>
                {
                    options.ClientId = "240475919297-mafpifo793qgthd1sat4ufn7gtfgfe8r.apps.googleusercontent.com";
                    options.ClientSecret = "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";
                    options.SignInScheme = CookieAuthenticationDefaults.AuthenticationScheme;
                    options.CallbackPath = "/signin-google-callback";
                    options.Scope.Add("openid");
                    options.Scope.Add("profile");
                    options.Scope.Add("email");
                    options.SaveTokens = true;
                });

            builder.Services.AddControllers().AddNewtonsoftJson();

            builder.Services.AddCors(options =>
                {
                    options.AddPolicy("AllowAngularApp", builder =>
                    {
                        builder.WithOrigins("http://localhost:4200") // Angular's URL
                        .AllowAnyHeader()
                        .AllowAnyMethod();
                    });
                });

            ConfigureMyServices(builder.Services);

            var app = builder.Build();

            await DataSeeder.SeedAsync(app.Services);

            // Configure the HTTP request pipeline.
            if (app.Environment.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                app.UseSwagger();
                app.UseSwaggerUI();
                //  app.ApplyMigration();
            }
            else
            {
                app.UseHsts();
            }

            app.UseHttpsRedirection();

            app.UseRouting();


            app.UseMiddleware<CustomExceptionMiddleware>();

            app.UseAuthentication();

            app.UseAuthorization();


            app.MapControllers();

            app.UseCors("AllowAngularApp");

            app.Run();
        }


        static void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();
            services.AddTransient<DDDSample1.Domain.User.IEmailSender, EmailSender>();

            services.AddTransient<IUserRepository, UserRepository>();
            services.AddTransient<IUserService, UserService>();

            services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
            services.AddTransient<OperationRequestService>();

            services.AddTransient<IOperationTypeRepository, OperationTypeRepository>();
            services.AddTransient<IOperationTypeService, OperationTypeService>();
            services.AddTransient<ISpecializationService, SpecializationService>();

            services.AddTransient<IPatientRepository, PatientRepository>();
            services.AddTransient<IPatientService, PatientService>();

            services.AddTransient<IMedicalHistoryRepository, MedicalHistoryRepository>();

            services.AddTransient<IOperationRequestService, OperationRequestService>();
            services.AddTransient<IDailyAvailabilityRepository, DailyAvailabilityRepository>();


            services.AddTransient<IStaffRepository, StaffRepository>();
            services.AddTransient<IStaffService, StaffService>();

            services.AddTransient<IAvailabilitySlotsRepository, AvailabilitySlotRepository>();


            services.AddTransient<ISpecializationRepository, SpecializationRepository>();

            services.AddTransient<IOperationRequestLoggerRepository, OperationRequestLoggerRepository>();
            services.AddTransient<OperationRequestLoggerService>();

            services.AddTransient<IPatientLoggerRepository, PatientLoggerRepository>();

            services.AddTransient<IAvailabilitySlotsRepository, AvailabilitySlotRepository>();
            services.AddTransient<AvailabilitySlotService>();

            services.AddTransient<IPhasesRepository, PhasesRepository>(); // Register IPhasesRepository

            services.AddTransient<IStaffLoggerRepository, StaffLoggerRepository>();
            services.AddTransient<StaffLoggerService>();

            services.AddTransient<IOperationRoomRepository, OperationRoomRepository>();
            services.AddTransient<IOperationRoomService, OperationRoomService>();

            services.AddTransient<IAppointmentRepository, AppointmentRepository>();
            services.AddTransient<IAppointmentService, AppointmentService>();
        }




    }


}
