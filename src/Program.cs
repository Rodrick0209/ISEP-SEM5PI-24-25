using Microsoft.AspNetCore.Builder;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Identity.UI.Services;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Categories;
using DDDSample1.Infrastructure.Products;
using DDDSample1.Infrastructure.Families;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Families;
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



var builder = WebApplication.CreateBuilder(args);

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(options => 
{
    options.AddSecurityDefinition(name : JwtBearerDefaults.AuthenticationScheme,
    securityScheme : new OpenApiSecurityScheme
    {
        Name = "Authorization",
        Description = "Enter the Bearer Authorization : 'Bearer Generated-JWT-Tplen'",
        In = ParameterLocation.Header,
        Type = SecuritySchemeType.ApiKey,
        Scheme = "Bearer"
    });
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
            new string[] {}
        }
    });




});



//CRIAR INTERFACE PARA TOKEN PROVIDER
builder.Services.AddSingleton<TokenProvider>(); 
// Add services to the container.
builder.Services.AddDbContext<DDDSample1DbContext>(opt =>
    opt.UseInMemoryDatabase("DDDSample1DB")
    .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

builder.Services.AddAuthorization();
builder.Services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
.AddJwtBearer(options =>
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
                
                // Configurar o código de status e a mensagem personalizada
                context.Response.StatusCode = StatusCodes.Status401Unauthorized;
                context.Response.ContentType = "application/json";
                
                var result = JsonSerializer.Serialize(new { message = "Token de autenticação inválido ou expirado. Por favor, faça login novamente." });
                return context.Response.WriteAsync(result);
            }
        };
    });

builder.Services.Configure<AuthMessageSenderOptions>(builder.Configuration.GetSection("SendGrid"));



builder.Services.AddControllers().AddNewtonsoftJson();





// Call the method to configure other services
ConfigureMyServices(builder.Services);

var app = builder.Build();

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

app.Run();

void ConfigureMyServices(IServiceCollection services)
{
    services.AddTransient<IUnitOfWork, UnitOfWork>();
    services.AddTransient<DDDSample1.Domain.User.IEmailSender, EmailSender>();
    
    services.AddTransient<ICategoryRepository, CategoryRepository>();
    services.AddTransient<CategoryService>();

    services.AddTransient<IProductRepository, ProductRepository>();
    services.AddTransient<ProductService>();

    services.AddTransient<IFamilyRepository, FamilyRepository>();
    services.AddTransient<FamilyService>();

    services.AddTransient<IUserRepository, UserRepository>();
    services.AddTransient<UserService>();

    services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
    services.AddTransient<OperationRequestService>();
}
