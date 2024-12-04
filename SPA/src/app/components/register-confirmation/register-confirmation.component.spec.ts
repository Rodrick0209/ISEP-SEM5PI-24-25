import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { RegisterConfirmationComponent } from './register-confirmation.component';
import { UserService } from '../../services/user.service';
import { CommonModule } from '@angular/common';

describe('RegisterConfirmationComponent', () => {
  let component: RegisterConfirmationComponent;
  let fixture: ComponentFixture<RegisterConfirmationComponent>;
  let userService: jasmine.SpyObj<UserService>;
  let router: jasmine.SpyObj<Router>;
  let activatedRoute: ActivatedRoute;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['confirmRegistration']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [RegisterConfirmationComponent, CommonModule],
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({ token: 'test-token', email: 'test@example.com' })
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(RegisterConfirmationComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRoute = TestBed.inject(ActivatedRoute);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should confirm registration on init', () => {
    const response = { message: 'Account confirmed' };
    userService.confirmRegistration.and.returnValue(of(response));

    fixture.detectChanges();

    expect(component.token).toBe('test-token');
    expect(component.email).toBe('test@example.com');
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-success'], {
      queryParams: { message: 'Your account has been confirmed succesfully.' }
    });
  });

  it('should handle registration confirmation error', () => {
    const errorResponse = { error: { message: 'Error occurred' } };
    userService.confirmRegistration.and.returnValue(throwError(errorResponse));

    fixture.detectChanges();

    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Error occurred' }
    });
  });

  it('should navigate to error page if token or email is missing', () => {
    activatedRoute.queryParams = of({ token: null, email: null });

    fixture.detectChanges();

    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Invalid confirmation link.' }
    });
  });
});