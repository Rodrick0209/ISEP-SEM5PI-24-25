import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { ConfirmationErrorComponent } from './confirmation-error.component';

describe('ConfirmationErrorComponent', () => {
  let component: ConfirmationErrorComponent;
  let fixture: ComponentFixture<ConfirmationErrorComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ConfirmationErrorComponent],
      providers: [
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({ message: 'Test error message' })
          }
        }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ConfirmationErrorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should display the error message from query params', () => {
    expect(component.errorMessage).toBe('Test error message');
  });


  it('should display default error message if no query param is provided', () => {
    TestBed.resetTestingModule();
    TestBed.configureTestingModule({
      imports: [ConfirmationErrorComponent],
      providers: [
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({})
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ConfirmationErrorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

    expect(component.errorMessage).toBe('An error occurred during confirmation.');
  });
});