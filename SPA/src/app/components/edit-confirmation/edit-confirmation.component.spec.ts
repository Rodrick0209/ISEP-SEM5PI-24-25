import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditConfirmationComponent } from './edit-confirmation.component';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CommonModule } from '@angular/common';



describe('EditConfirmationComponent', () => {
  let component: EditConfirmationComponent;
  let fixture: ComponentFixture<EditConfirmationComponent>;
  let userService: jasmine.SpyObj<UserService>;
  let router: jasmine.SpyObj<Router>;
  let activatedRoute: ActivatedRoute;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['confirmEdit']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [EditConfirmationComponent, CommonModule],
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({
              token: 'testToken',
              email: 'test@example.com',
              emailToEdit: 'new@example.com',
              phoneNumberToEdit: '1234567890'
            })
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditConfirmationComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRoute = TestBed.inject(ActivatedRoute);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with query params and call confirmEdit', () => {
    spyOn(component, 'confirmEdit');
    fixture.detectChanges();
    expect(component.token).toBe('testToken');
    expect(component.email).toBe('test@example.com');
    expect(component.emailToEdit).toBe('new@example.com');
    expect(component.phoneNumberToEdit).toBe('1234567890');
    expect(component.confirmEdit).toHaveBeenCalled();
  });

  it('should navigate to success page on successful edit', () => {
    userService.confirmEdit.and.returnValue(of({}));
    fixture.detectChanges();
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-success'], {
      queryParams: { message: 'Your account has been updated succesfully.' }
    });
  });

  it('should navigate to error page on edit failure', () => {
    userService.confirmEdit.and.returnValue(throwError({ error: { message: 'Error occurred' } }));
    fixture.detectChanges();
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Error occurred' }
    });
  });

  it('should navigate to error page if token or email is missing', () => {
    component.token = '';
    component.email = '';
    component.confirmEdit();
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Invalid token or email.' }
    });
  });
});
